use cranelift_module::DataId;
use gimli;
use std::collections::HashMap;
use std::path::Path;
use pasko_frontend::span;

pub struct DebugContext {
    endian: gimli::RunTimeEndian,

    dwarf: gimli::write::DwarfUnit,
    unit_range_list: gimli::write::RangeList,
    // created_files: FxHashMap<(StableSourceFileId, SourceFileHash), FileId>,
    stack_pointer_register: gimli::Register,
    array_size_type: gimli::write::UnitEntryId,
    file_id: gimli::write::FileId,
}
trait WriteDebugInfo {
    type SectionId: Copy;

    fn add_debug_section(&mut self, name: gimli::SectionId, data: Vec<u8>) -> Self::SectionId;
    fn add_debug_reloc(
        &mut self,
        section_map: &HashMap<gimli::SectionId, Self::SectionId>,
        from: &Self::SectionId,
        reloc: &DebugReloc,
    );
}

impl WriteDebugInfo for cranelift_object::ObjectProduct {
    type SectionId = (object::write::SectionId, object::write::SymbolId);

    fn add_debug_section(
        &mut self,
        id: gimli::SectionId,
        data: Vec<u8>,
    ) -> (object::write::SectionId, object::write::SymbolId) {
        let name = if self.object.format() == object::BinaryFormat::MachO {
            id.name().replace('.', "__") // machO expects __debug_info instead of .debug_info
        } else {
            id.name().to_string()
        }
        .into_bytes();

        let segment = self
            .object
            .segment_name(object::write::StandardSegment::Debug)
            .to_vec();
        // FIXME use SHT_X86_64_UNWIND for .eh_frame
        let section_id = self.object.add_section(
            segment,
            name,
            if id == gimli::SectionId::DebugStr || id == gimli::SectionId::DebugLineStr {
                object::SectionKind::DebugString
            } else if id == gimli::SectionId::EhFrame {
                object::SectionKind::ReadOnlyData
            } else {
                object::SectionKind::Debug
            },
        );
        self.object.section_mut(section_id).set_data(
            data,
            if id == gimli::SectionId::EhFrame {
                8
            } else {
                1
            },
        );
        let symbol_id = self.object.section_symbol(section_id);
        (section_id, symbol_id)
    }

    fn add_debug_reloc(
        &mut self,
        section_map: &HashMap<gimli::SectionId, Self::SectionId>,
        from: &Self::SectionId,
        reloc: &DebugReloc,
    ) {
        let (symbol, symbol_offset) = match reloc.name {
            DebugRelocName::Section(id) => (section_map.get(&id).unwrap().1, 0),
            DebugRelocName::Symbol(id) => {
                let id = id.try_into().unwrap();
                let symbol_id = if id & 1 << 31 == 0 {
                    self.function_symbol(cranelift_module::FuncId::from_u32(id))
                } else {
                    self.data_symbol(DataId::from_u32(id & !(1 << 31)))
                };
                self.object
                    .symbol_section_and_offset(symbol_id)
                    .unwrap_or((symbol_id, 0))
            }
        };
        self.object
            .add_relocation(
                from.0,
                object::write::Relocation {
                    offset: u64::from(reloc.offset),
                    symbol,
                    flags: object::write::RelocationFlags::Generic {
                        kind: reloc.kind,
                        encoding: object::write::RelocationEncoding::Generic,
                        size: reloc.size * 8,
                    },
                    addend: i64::try_from(symbol_offset).unwrap() + reloc.addend,
                },
            )
            .unwrap();
    }
}

#[derive(Clone)]
struct DebugReloc {
    offset: u32,
    size: u8,
    name: DebugRelocName,
    addend: i64,
    kind: object::RelocationKind,
}

#[derive(Clone)]
enum DebugRelocName {
    Section(gimli::SectionId),
    Symbol(usize),
}

/// A [`Writer`] that collects all necessary relocations.
#[derive(Clone)]
struct WriterRelocate {
    relocs: Vec<DebugReloc>,
    writer: gimli::write::EndianVec<gimli::RunTimeEndian>,
}

impl WriterRelocate {
    fn new(endian: gimli::RunTimeEndian) -> Self {
        WriterRelocate {
            relocs: Vec::new(),
            writer: gimli::write::EndianVec::new(endian),
        }
    }
}

impl gimli::write::Writer for WriterRelocate {
    type Endian = gimli::RunTimeEndian;

    fn endian(&self) -> Self::Endian {
        self.writer.endian()
    }

    fn len(&self) -> usize {
        self.writer.len()
    }

    fn write(&mut self, bytes: &[u8]) -> gimli::write::Result<()> {
        self.writer.write(bytes)
    }

    fn write_at(&mut self, offset: usize, bytes: &[u8]) -> gimli::write::Result<()> {
        self.writer.write_at(offset, bytes)
    }

    fn write_address(
        &mut self,
        address: gimli::write::Address,
        size: u8,
    ) -> gimli::write::Result<()> {
        match address {
            gimli::write::Address::Constant(val) => self.write_udata(val, size),
            gimli::write::Address::Symbol { symbol, addend } => {
                let offset = self.len() as u64;
                self.relocs.push(DebugReloc {
                    offset: offset as u32,
                    size,
                    name: DebugRelocName::Symbol(symbol),
                    addend,
                    kind: object::RelocationKind::Absolute,
                });
                self.write_udata(0, size)
            }
        }
    }

    fn write_offset(
        &mut self,
        val: usize,
        section: gimli::SectionId,
        size: u8,
    ) -> gimli::write::Result<()> {
        let offset = self.len() as u32;
        self.relocs.push(DebugReloc {
            offset,
            size,
            name: DebugRelocName::Section(section),
            addend: val as i64,
            kind: object::RelocationKind::Absolute,
        });
        self.write_udata(0, size)
    }

    fn write_offset_at(
        &mut self,
        offset: usize,
        val: usize,
        section: gimli::SectionId,
        size: u8,
    ) -> gimli::write::Result<()> {
        self.relocs.push(DebugReloc {
            offset: offset as u32,
            size,
            name: DebugRelocName::Section(section),
            addend: val as i64,
            kind: object::RelocationKind::Absolute,
        });
        self.write_udata_at(offset, 0, size)
    }

    fn write_eh_pointer(
        &mut self,
        address: gimli::write::Address,
        eh_pe: gimli::DwEhPe,
        size: u8,
    ) -> gimli::write::Result<()> {
        match address {
            // Address::Constant arm copied from gimli
            gimli::write::Address::Constant(val) => {
                // Indirect doesn't matter here.
                let val = match eh_pe.application() {
                    gimli::DW_EH_PE_absptr => val,
                    gimli::DW_EH_PE_pcrel => {
                        // FIXME better handling of sign
                        let offset = self.len() as u64;
                        offset.wrapping_sub(val)
                    }
                    _ => {
                        return Err(gimli::write::Error::UnsupportedPointerEncoding(eh_pe));
                    }
                };
                self.write_eh_pointer_data(val, eh_pe.format(), size)
            }
            gimli::write::Address::Symbol { symbol, addend } => match eh_pe.application() {
                gimli::DW_EH_PE_pcrel => {
                    let size = match eh_pe.format() {
                        gimli::DW_EH_PE_sdata4 => 4,
                        gimli::DW_EH_PE_sdata8 => 8,
                        _ => return Err(gimli::write::Error::UnsupportedPointerEncoding(eh_pe)),
                    };
                    self.relocs.push(DebugReloc {
                        offset: self.len() as u32,
                        size,
                        name: DebugRelocName::Symbol(symbol),
                        addend,
                        kind: object::RelocationKind::Relative,
                    });
                    self.write_udata(0, size)
                }
                gimli::DW_EH_PE_absptr => {
                    self.relocs.push(DebugReloc {
                        offset: self.len() as u32,
                        size: size.into(),
                        name: DebugRelocName::Symbol(symbol),
                        addend,
                        kind: object::RelocationKind::Absolute,
                    });
                    self.write_udata(0, size.into())
                }
                _ => Err(gimli::write::Error::UnsupportedPointerEncoding(eh_pe)),
            },
        }
    }
}

pub fn init_debug_context(source_filename: &Path, address_size: u8) -> DebugContext {
    // Inspired by rustc_codegen_cranelift
    let encoding = gimli::Encoding {
        format: gimli::Format::Dwarf32,
        version: 4,
        address_size,
    };

    let endian = gimli::RunTimeEndian::Little;

    // FIXME: Stack register
    let stack_pointer_register = gimli::X86_64::RSP;

    let mut dwarf = gimli::write::DwarfUnit::new(encoding);

    let producer = "pasko".to_string();
    // FIXME: Compute this in the driver.
    let comp_dir = std::env::current_dir().unwrap();
    let comp_dir = comp_dir.to_string_lossy().to_string();
    let file_name = "hello.pas".to_string();

    let file_info = None;
    let line_program = gimli::write::LineProgram::new(
        encoding,
        gimli::LineEncoding::default(),
        gimli::write::LineString::new(comp_dir.as_bytes(), encoding, &mut dwarf.line_strings),
        gimli::write::LineString::new(file_name.as_bytes(), encoding, &mut dwarf.line_strings),
        file_info,
    );
    dwarf.unit.line_program = line_program;

    {
        let file_name = dwarf.strings.add(file_name);
        let comp_dir = dwarf.strings.add(comp_dir);

        let root = dwarf.unit.root();
        let root = dwarf.unit.get_mut(root);

        root.set(
            gimli::DW_AT_producer,
            gimli::write::AttributeValue::StringRef(dwarf.strings.add(producer)),
        );
        root.set(
            gimli::DW_AT_language,
            gimli::write::AttributeValue::Language(gimli::DW_LANG_Pascal83),
        );
        root.set(
            gimli::DW_AT_name,
            gimli::write::AttributeValue::StringRef(file_name),
        );
        root.set(
            gimli::DW_AT_comp_dir,
            gimli::write::AttributeValue::StringRef(comp_dir),
        );
        root.set(
            gimli::DW_AT_low_pc,
            gimli::write::AttributeValue::Address(gimli::write::Address::Constant(0)),
        );
    }

    let array_size_type = dwarf.unit.add(dwarf.unit.root(), gimli::DW_TAG_base_type);
    let array_size_type_entry = dwarf.unit.get_mut(array_size_type);
    array_size_type_entry.set(
        gimli::DW_AT_name,
        gimli::write::AttributeValue::StringRef(dwarf.strings.add("__ARRAY_SIZE_TYPE__")),
    );
    array_size_type_entry.set(
        gimli::DW_AT_encoding,
        gimli::write::AttributeValue::Encoding(gimli::DW_ATE_unsigned),
    );
    array_size_type_entry.set(
        gimli::DW_AT_byte_size,
        gimli::write::AttributeValue::Udata(address_size as u64),
    );

    let default_dir = dwarf.unit.line_program.default_directory();
    use std::os::unix::ffi::OsStrExt;
    let file_id = dwarf.unit.line_program.add_file(
        gimli::write::LineString::new(
            source_filename.as_os_str().as_bytes(),
            encoding,
            &mut dwarf.line_strings,
        ),
        default_dir,
        None,
    );

    DebugContext {
        endian,
        dwarf,
        unit_range_list: gimli::write::RangeList(Vec::new()),
        stack_pointer_register,
        array_size_type,
        file_id,
    }
}

pub fn emit_debug_information(
    debug_context: &mut DebugContext,
    product: &mut cranelift_object::ObjectProduct,
) {
    let unit_range_list_id = debug_context
        .dwarf
        .unit
        .ranges
        .add(debug_context.unit_range_list.clone());
    let root = debug_context.dwarf.unit.root();
    let root = debug_context.dwarf.unit.get_mut(root);
    root.set(
        gimli::DW_AT_ranges,
        gimli::write::AttributeValue::RangeListRef(unit_range_list_id),
    );

    let mut sections = gimli::write::Sections::new(WriterRelocate::new(debug_context.endian));
    debug_context.dwarf.write(&mut sections).unwrap();

    let mut section_map = HashMap::default();
    let _: gimli::write::Result<()> = sections.for_each_mut(|id, section| {
        if !section.writer.slice().is_empty() {
            let section_id = product.add_debug_section(id, section.writer.take());
            section_map.insert(id, section_id);
        }
        Ok(())
    });

    let _: gimli::write::Result<()> = sections.for_each(|id, section| {
        if let Some(section_id) = section_map.get(&id) {
            for reloc in &section.relocs {
                product.add_debug_reloc(&section_map, section_id, reloc);
            }
        }
        Ok(())
    });
}

fn address_for_func(func_id: cranelift_module::FuncId) -> gimli::write::Address {
    let symbol = func_id.as_u32();
    assert!(symbol & 1 << 31 == 0);
    gimli::write::Address::Symbol {
        symbol: symbol as usize,
        addend: 0,
    }
}

pub fn create_debug_lines(
    debug_context: &mut DebugContext,
    func_id: cranelift_module::FuncId,
    function_name: &str,
    function_location: &span::SpanLoc,
    mcr: &cranelift_codegen::CompiledCode,
    linemap: &span::LineMap,
) {
    // Inspired by rustc_codegen_cranelift
    let create_row_for_span =
        |debug_context: &mut DebugContext, source_loc: (gimli::write::FileId, u64, u64)| {
            let (file_id, line, col) = source_loc;

            debug_context.dwarf.unit.line_program.row().file = file_id;
            debug_context.dwarf.unit.line_program.row().line = line;
            debug_context.dwarf.unit.line_program.row().column = col;
            debug_context.dwarf.unit.line_program.generate_row();
        };

    // FIXME: nested functions!!!
    let root = debug_context.dwarf.unit.root();
    let entry_id = debug_context.dwarf.unit.add(root, gimli::DW_TAG_subprogram);
    let entry = debug_context.dwarf.unit.get_mut(entry_id);
    let name_id = debug_context.dwarf.strings.add(function_name);
    entry.set(
        gimli::DW_AT_name,
        gimli::write::AttributeValue::StringRef(name_id),
    );

    debug_context
        .dwarf
        .unit
        .line_program
        .begin_sequence(Some(address_for_func(func_id)));

    let mut func_end = 0;

    let mut current_loc = function_location.begin();
    // let mcr = self.ctx.compiled_code().unwrap();
    for &cranelift_codegen::MachSrcLoc { start, end, loc } in mcr.buffer.get_srclocs_sorted() {
        debug_context.dwarf.unit.line_program.row().address_offset = u64::from(start);
        if !loc.is_default() {
            current_loc = loc.bits() as usize;
        }
        let (line, col) = linemap.offset_to_line_and_col(current_loc);
        let source_loc = (debug_context.file_id, line as u64, col as u64);
        create_row_for_span(debug_context, source_loc);
        func_end = end;
    }

    debug_context
        .dwarf
        .unit
        .line_program
        .end_sequence(u64::from(func_end));

    let func_end = mcr.buffer.total_size();

    assert_ne!(func_end, 0);

    let entry = debug_context.dwarf.unit.get_mut(entry_id);
    entry.set(
        gimli::DW_AT_low_pc,
        gimli::write::AttributeValue::Address(address_for_func(func_id)),
    );
    entry.set(
        gimli::DW_AT_high_pc,
        gimli::write::AttributeValue::Udata(u64::from(func_end)),
    );
}
