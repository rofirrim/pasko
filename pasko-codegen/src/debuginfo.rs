use crate::program::OffsetCache;
use crate::program::SizeAndAlignmentCache;
use cranelift_module::DataId;
use gimli;
use pasko_frontend::span;
use pasko_frontend::symbol::SymbolId;
use std::collections::HashMap;
use std::path::Path;

use log::info;

// Inspired by rustc_codegen_cranelift

pub struct DebugContext {
    endian: gimli::RunTimeEndian,

    dwarf: gimli::write::DwarfUnit,
    unit_range_list: gimli::write::RangeList,
    stack_pointer_register: gimli::Register,
    file_id: gimli::write::FileId,

    main_function_entry_id: Option<(gimli::write::UnitEntryId, String)>,
    function_entry_id:
        HashMap<pasko_frontend::symbol::SymbolId, (gimli::write::UnitEntryId, String)>,
    type_map: HashMap<pasko_frontend::typesystem::TypeId, gimli::write::UnitEntryId>,

    set_type: Option<gimli::write::UnitEntryId>,
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

impl DebugContext {
    pub fn init_debug_context(source_filename: &Path, address_size: u8) -> DebugContext {
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

        let file_name = source_filename.as_os_str().as_bytes();

        let file_info = None;
        let line_program = gimli::write::LineProgram::new(
            encoding,
            gimli::LineEncoding::default(),
            gimli::write::LineString::new(comp_dir.as_bytes(), encoding, &mut dwarf.line_strings),
            gimli::write::LineString::new(file_name, encoding, &mut dwarf.line_strings),
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

        let default_dir = dwarf.unit.line_program.default_directory();
        use std::os::unix::ffi::OsStrExt;
        let file_id = dwarf.unit.line_program.add_file(
            gimli::write::LineString::new(file_name, encoding, &mut dwarf.line_strings),
            default_dir,
            None,
        );

        DebugContext {
            endian,
            dwarf,
            unit_range_list: gimli::write::RangeList(Vec::new()),
            stack_pointer_register,
            // array_size_type,
            file_id,
            main_function_entry_id: None,
            function_entry_id: HashMap::new(),
            type_map: HashMap::new(),
            set_type: None,
        }
    }

    pub fn emit_debug_information(&mut self, product: &mut cranelift_object::ObjectProduct) {
        let unit_range_list_id = self.dwarf.unit.ranges.add(self.unit_range_list.clone());
        let root = self.dwarf.unit.root();
        let root = self.dwarf.unit.get_mut(root);
        root.set(
            gimli::DW_AT_ranges,
            gimli::write::AttributeValue::RangeListRef(unit_range_list_id),
        );

        let mut sections = gimli::write::Sections::new(WriterRelocate::new(self.endian));
        self.dwarf.write(&mut sections).unwrap();

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

    pub fn create_debug_lines(
        &mut self,
        semantic_context: &pasko_frontend::semantic::SemanticContext,
        function_symbol_id: Option<pasko_frontend::symbol::SymbolId>,
        func_id: cranelift_module::FuncId,
        function_location: &span::SpanLoc,
        mcr: &cranelift_codegen::CompiledCode,
        linemap: &span::LineMap,
    ) {
        let entry_id = self.define_function(
            semantic_context,
            function_symbol_id,
            function_location,
            linemap,
        );

        let create_row_for_span =
            |debug_context: &mut DebugContext, source_loc: (gimli::write::FileId, u64, u64)| {
                let (file_id, line, col) = source_loc;

                debug_context.dwarf.unit.line_program.row().file = file_id;
                debug_context.dwarf.unit.line_program.row().line = line;
                debug_context.dwarf.unit.line_program.row().column = col;
                debug_context.dwarf.unit.line_program.generate_row();
            };

        self.dwarf
            .unit
            .line_program
            .begin_sequence(Some(address_for_func(func_id)));

        let mut func_end = 0;

        let mut current_loc = function_location.begin();
        // let mcr = self.ctx.compiled_code().unwrap();
        for &cranelift_codegen::MachSrcLoc { start, end, loc } in mcr.buffer.get_srclocs_sorted() {
            self.dwarf.unit.line_program.row().address_offset = u64::from(start);
            if !loc.is_default() {
                current_loc = loc.bits() as usize;
            }
            let (line, col) = linemap.offset_to_line_and_col(current_loc);
            let source_loc = (self.file_id, line as u64, col as u64);
            create_row_for_span(self, source_loc);
            func_end = end;
        }

        self.dwarf
            .unit
            .line_program
            .end_sequence(u64::from(func_end));

        let func_end = mcr.buffer.total_size();

        assert_ne!(func_end, 0);

        let entry = self.dwarf.unit.get_mut(entry_id);
        entry.set(
            gimli::DW_AT_low_pc,
            gimli::write::AttributeValue::Address(address_for_func(func_id)),
        );
        entry.set(
            gimli::DW_AT_high_pc,
            gimli::write::AttributeValue::Udata(u64::from(func_end)),
        );
    }

    fn get_main_function_entry_id(self: &mut DebugContext) -> (gimli::write::UnitEntryId, String) {
        if let Some(entry) = self.main_function_entry_id.clone() {
            return entry;
        }

        let parent_unit_id = self.dwarf.unit.root();

        let ret = (
            self.dwarf
                .unit
                .add(parent_unit_id, gimli::DW_TAG_subprogram),
            "main".to_string(),
        );
        self.main_function_entry_id = Some(ret.clone());

        ret
    }

    fn get_function_entry_id(
        &mut self,
        semantic_context: &pasko_frontend::semantic::SemanticContext,
        function_symbol_id: pasko_frontend::symbol::SymbolId,
    ) -> (gimli::write::UnitEntryId, String) {
        if let Some(entry) = self.function_entry_id.get(&function_symbol_id) {
            return entry.clone();
        }

        let mut parent_unit_id = self.dwarf.unit.root();

        let mut function_name;

        let function_symbol = semantic_context.get_symbol(function_symbol_id);
        let function_symbol = function_symbol.borrow();
        let function_symbol_scope = function_symbol.get_scope().unwrap();

        function_name = function_symbol.get_name().clone();
        if let Some(enclosing_function) = semantic_context
            .scope
            .get_innermost_scope_symbol(function_symbol_scope)
        {
            let parent_function_name: String;
            (parent_unit_id, parent_function_name) =
                self.get_function_entry_id(semantic_context, enclosing_function);

            function_name = format!("{}:{}", parent_function_name, function_name);
        }

        let ret = (
            self.dwarf
                .unit
                .add(parent_unit_id, gimli::DW_TAG_subprogram),
            function_name,
        );

        self.function_entry_id
            .insert(function_symbol_id, ret.clone());

        ret
    }

    fn define_function(
        &mut self,
        semantic_context: &pasko_frontend::semantic::SemanticContext,
        function_symbol_id: Option<pasko_frontend::symbol::SymbolId>,
        function_location: &span::SpanLoc,
        linemap: &span::LineMap,
    ) -> gimli::write::UnitEntryId {
        let (line, col) = linemap.offset_to_line_and_col(function_location.begin());

        let (entry_id, function_name) = if let Some(function_symbol_id) = function_symbol_id {
            self.get_function_entry_id(semantic_context, function_symbol_id)
        } else {
            self.get_main_function_entry_id()
        };

        let entry = self.dwarf.unit.get_mut(entry_id);
        let name_id = self.dwarf.strings.add(function_name);
        entry.set(
            gimli::DW_AT_name,
            gimli::write::AttributeValue::StringRef(name_id),
        );

        entry.set(
            gimli::DW_AT_decl_file,
            gimli::write::AttributeValue::FileIndex(Some(self.file_id)),
        );
        entry.set(
            gimli::DW_AT_decl_line,
            gimli::write::AttributeValue::Udata(line as u64),
        );
        entry.set(
            gimli::DW_AT_decl_column,
            gimli::write::AttributeValue::Udata(col as u64),
        );

        let mut frame_base_expr = gimli::write::Expression::new();
        frame_base_expr.op_reg(self.stack_pointer_register);
        entry.set(
            gimli::DW_AT_frame_base,
            gimli::write::AttributeValue::Exprloc(frame_base_expr),
        );

        entry_id
    }

    pub fn define_global_variable(
        &mut self,
        semantic_context: &pasko_frontend::semantic::SemanticContext,
        symbol_name: &String,
        symbol_type: pasko_frontend::typesystem::TypeId,
        align: u64,
        data_id: cranelift_module::DataId,
        defining_point: &span::SpanLoc,
        linemap: &span::LineMap,
        size_and_alignment_cache: &SizeAndAlignmentCache,
        offset_cache: &OffsetCache,
    ) {
        let scope = self.dwarf.unit.root();

        let (line, col) = linemap.offset_to_line_and_col(defining_point.begin());
        let entry_id = self.dwarf.unit.add(scope, gimli::DW_TAG_variable);

        let type_id = self.debug_type(
            semantic_context,
            symbol_type,
            size_and_alignment_cache,
            offset_cache,
        );

        let entry = self.dwarf.unit.get_mut(entry_id);
        let name_id = self.dwarf.strings.add(symbol_name.as_str());
        entry.set(
            gimli::DW_AT_name,
            gimli::write::AttributeValue::StringRef(name_id),
        );

        entry.set(
            gimli::DW_AT_decl_file,
            gimli::write::AttributeValue::FileIndex(Some(self.file_id)),
        );
        entry.set(
            gimli::DW_AT_decl_line,
            gimli::write::AttributeValue::Udata(line as u64),
        );
        entry.set(
            gimli::DW_AT_decl_column,
            gimli::write::AttributeValue::Udata(col as u64),
        );

        entry.set(
            gimli::DW_AT_alignment,
            gimli::write::AttributeValue::Udata(align),
        );

        let mut expr = gimli::write::Expression::new();
        expr.op_addr(address_for_data(data_id));
        entry.set(
            gimli::DW_AT_location,
            gimli::write::AttributeValue::Exprloc(expr),
        );

        entry.set(
            gimli::DW_AT_type,
            gimli::write::AttributeValue::UnitRef(type_id),
        );
    }

    fn debug_type(
        &mut self,
        semantic_context: &pasko_frontend::semantic::SemanticContext,
        ty: pasko_frontend::typesystem::TypeId,
        size_and_alignment_cache: &SizeAndAlignmentCache,
        offset_cache: &OffsetCache,
    ) -> gimli::write::UnitEntryId {
        if let Some(entry_id) = self.type_map.get(&ty) {
            return *entry_id;
        }

        let type_id = if semantic_context.type_system.is_named_type(ty) {
            self.typedef_type(semantic_context, ty, size_and_alignment_cache, offset_cache)
        } else if semantic_context.type_system.is_integer_type(ty)
            || semantic_context.type_system.is_bool_type(ty)
            || semantic_context.type_system.is_real_type(ty)
            || semantic_context.type_system.is_char_type(ty)
        {
            self.basic_type(semantic_context, ty)
        } else if semantic_context.type_system.is_subrange_type(ty) {
            self.subrange_type(
                semantic_context,
                ty,
                None,
                size_and_alignment_cache,
                offset_cache,
            )
        } else if semantic_context.type_system.is_enum_type(ty) {
            self.enum_type(
                semantic_context,
                ty,
                None,
                size_and_alignment_cache,
                offset_cache,
            )
        } else if semantic_context.type_system.is_array_type(ty) {
            self.array_type(semantic_context, ty, size_and_alignment_cache, offset_cache)
        } else if semantic_context.type_system.is_record_type(ty) {
            self.record_type(semantic_context, ty, size_and_alignment_cache, offset_cache)
        } else if semantic_context.type_system.is_pointer_type(ty) {
            self.pointer_type(semantic_context, ty, size_and_alignment_cache, offset_cache)
        } else if semantic_context.type_system.is_set_type(ty) {
            self.set_type(semantic_context, ty, size_and_alignment_cache, offset_cache)
        } else {
            info!(
                "unimplemented debugging information support for type {}",
                semantic_context.type_system.get_type_name(ty)
            );

            self.unspecified_type(semantic_context, ty, size_and_alignment_cache, offset_cache)
        };

        self.type_map.insert(ty, type_id);

        type_id
    }

    fn basic_type_impl(
        &mut self,
        name: &str,
        bytes: u64,
        encoding: gimli::constants::DwAte,
    ) -> gimli::write::UnitEntryId {
        let type_id = self
            .dwarf
            .unit
            .add(self.dwarf.unit.root(), gimli::DW_TAG_base_type);
        let type_entry = self.dwarf.unit.get_mut(type_id);
        type_entry.set(
            gimli::DW_AT_name,
            gimli::write::AttributeValue::StringRef(self.dwarf.strings.add(name)),
        );
        type_entry.set(
            gimli::DW_AT_encoding,
            gimli::write::AttributeValue::Encoding(encoding),
        );
        type_entry.set(
            gimli::DW_AT_byte_size,
            gimli::write::AttributeValue::Udata(bytes),
        );
        type_id
    }

    fn basic_type(
        &mut self,
        semantic_context: &pasko_frontend::semantic::SemanticContext,
        ty: pasko_frontend::typesystem::TypeId,
    ) -> gimli::write::UnitEntryId {
        let type_id = if semantic_context.type_system.is_integer_type(ty) {
            self.basic_type_impl("integer", 8, gimli::DW_ATE_signed)
        } else if semantic_context.type_system.is_bool_type(ty) {
            self.basic_type_impl("boolean", 1, gimli::DW_ATE_boolean)
        } else if semantic_context.type_system.is_real_type(ty) {
            self.basic_type_impl("real", 8, gimli::DW_ATE_float)
        } else if semantic_context.type_system.is_char_type(ty) {
            self.basic_type_impl("char", 4, gimli::DW_ATE_UTF)
        } else {
            panic!("Unexpected basic type");
        };

        type_id
    }

    fn subrange_type(
        &mut self,
        semantic_context: &pasko_frontend::semantic::SemanticContext,
        ty: pasko_frontend::typesystem::TypeId,
        parent: Option<gimli::write::UnitEntryId>,
        size_and_alignment_cache: &SizeAndAlignmentCache,
        offset_cache: &OffsetCache,
    ) -> gimli::write::UnitEntryId {
        let base_type = self.debug_type(
            semantic_context,
            semantic_context.type_system.get_integer_type(),
            size_and_alignment_cache,
            offset_cache,
        );
        let lower = semantic_context.type_system.ordinal_type_lower_bound(ty);
        let upper = semantic_context.type_system.ordinal_type_upper_bound(ty);
        let type_id = self.dwarf.unit.add(
            parent.unwrap_or_else(|| self.dwarf.unit.root()),
            gimli::DW_TAG_subrange_type,
        );
        let entry = self.dwarf.unit.get_mut(type_id);
        entry.set(
            gimli::DW_AT_type,
            gimli::write::AttributeValue::UnitRef(base_type),
        );
        entry.set(
            gimli::DW_AT_lower_bound,
            gimli::write::AttributeValue::Sdata(lower),
        );
        entry.set(
            gimli::DW_AT_upper_bound,
            gimli::write::AttributeValue::Sdata(upper),
        );
        type_id
    }

    fn enum_type(
        &mut self,
        semantic_context: &pasko_frontend::semantic::SemanticContext,
        ty: pasko_frontend::typesystem::TypeId,
        parent: Option<gimli::write::UnitEntryId>,
        size_and_alignment_cache: &SizeAndAlignmentCache,
        offset_cache: &OffsetCache,
    ) -> gimli::write::UnitEntryId {
        let base_type = self.debug_type(
            semantic_context,
            semantic_context.type_system.get_integer_type(),
            size_and_alignment_cache,
            offset_cache,
        );
        let type_id = self.dwarf.unit.add(
            parent.unwrap_or_else(|| self.dwarf.unit.root()),
            gimli::DW_TAG_enumeration_type,
        );

        let enumerators = semantic_context.type_system.enum_type_get_enumerators(ty);
        enumerators
            .iter()
            .enumerate()
            .for_each(|(value, enumerator)| {
                let enum_entry = self.dwarf.unit.add(type_id, gimli::DW_TAG_enumerator);
                let entry = self.dwarf.unit.get_mut(enum_entry);
                let enumerator_sym = semantic_context.get_symbol(*enumerator);
                let enumerator_sym = enumerator_sym.borrow();
                let enumerator_name = enumerator_sym.get_name();
                entry.set(
                    gimli::DW_AT_name,
                    gimli::write::AttributeValue::StringRef(
                        self.dwarf.strings.add(enumerator_name.as_str()),
                    ),
                );
                entry.set(
                    gimli::DW_AT_const_value,
                    gimli::write::AttributeValue::Sdata(value as i64),
                );
            });

        let entry = self.dwarf.unit.get_mut(type_id);
        entry.set(
            gimli::DW_AT_type,
            gimli::write::AttributeValue::UnitRef(base_type),
        );

        type_id
    }

    fn array_type(
        &mut self,
        semantic_context: &pasko_frontend::semantic::SemanticContext,
        ty: pasko_frontend::typesystem::TypeId,
        size_and_alignment_cache: &SizeAndAlignmentCache,
        offset_cache: &OffsetCache,
    ) -> gimli::write::UnitEntryId {
        let type_id = self
            .dwarf
            .unit
            .add(self.dwarf.unit.root(), gimli::DW_TAG_array_type);
        let mut component_type = ty;
        while semantic_context.type_system.is_array_type(component_type) {
            let index_type = semantic_context.type_system.array_type_get_index_type(ty);
            if semantic_context.type_system.is_enum_type(index_type) {
                self.enum_type(
                    semantic_context,
                    index_type,
                    Some(type_id),
                    size_and_alignment_cache,
                    offset_cache,
                );
            } else if semantic_context.type_system.is_subrange_type(index_type) {
                self.subrange_type(
                    semantic_context,
                    index_type,
                    Some(type_id),
                    size_and_alignment_cache,
                    offset_cache,
                );
            } else {
                panic!("Unexpected type for array index");
            }
            component_type = semantic_context
                .type_system
                .array_type_get_component_type(component_type);
        }
        let base_type = self.debug_type(
            semantic_context,
            component_type,
            size_and_alignment_cache,
            offset_cache,
        );

        let entry = self.dwarf.unit.get_mut(type_id);
        entry.set(
            gimli::DW_AT_type,
            gimli::write::AttributeValue::UnitRef(base_type),
        );
        entry.set(
            gimli::DW_AT_ordering,
            gimli::write::AttributeValue::Ordering(gimli::DW_ORD_row_major),
        );

        type_id
    }

    fn record_type(
        &mut self,
        semantic_context: &pasko_frontend::semantic::SemanticContext,
        ty: pasko_frontend::typesystem::TypeId,
        size_and_alignment_cache: &SizeAndAlignmentCache,
        offset_cache: &OffsetCache,
    ) -> gimli::write::UnitEntryId {
        let type_id = self
            .dwarf
            .unit
            .add(self.dwarf.unit.root(), gimli::DW_TAG_structure_type);

        let fields = semantic_context
            .type_system
            .record_type_get_fixed_fields(ty);
        if semantic_context
            .type_system
            .record_type_get_variant_part(ty)
            .is_some()
        {
            unimplemented!("Variant records");
        }
        fields.iter().for_each(|field_sym_id| {
            let field_sym = semantic_context.get_symbol(*field_sym_id);
            let field_sym = field_sym.borrow();
            let field_entry_id = self.dwarf.unit.add(type_id, gimli::DW_TAG_member);

            let field_type_id = self.debug_type(
                semantic_context,
                field_sym.get_type().unwrap(),
                size_and_alignment_cache,
                offset_cache,
            );

            let field_entry = self.dwarf.unit.get_mut(field_entry_id);
            field_entry.set(
                gimli::DW_AT_name,
                gimli::write::AttributeValue::StringRef(
                    self.dwarf.strings.add(field_sym.get_name().as_str()),
                ),
            );

            field_entry.set(
                gimli::DW_AT_type,
                gimli::write::AttributeValue::UnitRef(field_type_id),
            );

            if let Some(offset) = offset_cache.borrow().get(&*field_sym_id) {
                field_entry.set(
                    gimli::DW_AT_data_member_location,
                    gimli::write::AttributeValue::Udata(*offset as u64),
                )
            }
        });

        let entry = self.dwarf.unit.get_mut(type_id);
        if let Some(info) = size_and_alignment_cache.borrow().get(&ty) {
            entry.set(
                gimli::DW_AT_byte_size,
                gimli::write::AttributeValue::Udata(info.size as u64),
            );
        }

        type_id
    }

    fn pointer_type(
        &mut self,
        semantic_context: &pasko_frontend::semantic::SemanticContext,
        ty: pasko_frontend::typesystem::TypeId,
        size_and_alignment_cache: &SizeAndAlignmentCache,
        offset_cache: &OffsetCache,
    ) -> gimli::write::UnitEntryId {
        let type_id = self
            .dwarf
            .unit
            .add(self.dwarf.unit.root(), gimli::DW_TAG_pointer_type);

        let pointee_type = semantic_context
            .type_system
            .pointer_type_get_pointee_type(ty);
        let pointee_type_id = self.debug_type(
            semantic_context,
            pointee_type,
            size_and_alignment_cache,
            offset_cache,
        );

        let entry = self.dwarf.unit.get_mut(type_id);
        entry.set(
            gimli::DW_AT_type,
            gimli::write::AttributeValue::UnitRef(pointee_type_id),
        );
        let entry = self.dwarf.unit.get_mut(type_id);
        if let Some(info) = size_and_alignment_cache.borrow().get(&ty) {
            entry.set(
                gimli::DW_AT_byte_size,
                gimli::write::AttributeValue::Udata(info.size as u64),
            );
        }

        type_id
    }

    fn set_type_enum_mode(&mut self) -> gimli::write::UnitEntryId {
        let type_id = self
            .dwarf
            .unit
            .add(self.dwarf.unit.root(), gimli::DW_TAG_enumeration_type);

        // SM_BITMAP = 0
        let enum_entry = self.dwarf.unit.add(type_id, gimli::DW_TAG_enumerator);
        let entry = self.dwarf.unit.get_mut(enum_entry);
        entry.set(
            gimli::DW_AT_name,
            gimli::write::AttributeValue::StringRef(self.dwarf.strings.add("SM_BITMAP")),
        );
        entry.set(
            gimli::DW_AT_const_value,
            gimli::write::AttributeValue::Sdata(0),
        );
        // SM_SORTED_ARRAY = 1
        let enum_entry = self.dwarf.unit.add(type_id, gimli::DW_TAG_enumerator);
        let entry = self.dwarf.unit.get_mut(enum_entry);
        entry.set(
            gimli::DW_AT_name,
            gimli::write::AttributeValue::StringRef(self.dwarf.strings.add("SM_SORTED_ARRAY")),
        );
        entry.set(
            gimli::DW_AT_const_value,
            gimli::write::AttributeValue::Sdata(1),
        );

        let base_type = self.basic_type_impl("integer", 4, gimli::DW_ATE_signed);
        let entry = self.dwarf.unit.get_mut(type_id);
        entry.set(
            gimli::DW_AT_type,
            gimli::write::AttributeValue::UnitRef(base_type),
        );

        type_id
    }

    fn set_type_bitmap(&mut self) -> gimli::write::UnitEntryId {
        let type_id = self
            .dwarf
            .unit
            .add(self.dwarf.unit.root(), gimli::DW_TAG_structure_type);

        let base_type = self.basic_type_impl("uint64_t", 8, gimli::DW_ATE_unsigned);

        let array_type_id = {
            let type_id = self
                .dwarf
                .unit
                .add(self.dwarf.unit.root(), gimli::DW_TAG_array_type);

            let entry = self.dwarf.unit.get_mut(type_id);
            entry.set(
                gimli::DW_AT_type,
                gimli::write::AttributeValue::UnitRef(base_type),
            );
            entry.set(
                gimli::DW_AT_ordering,
                gimli::write::AttributeValue::Ordering(gimli::DW_ORD_row_major),
            );

            type_id
        };

        let _subrange_type = {
            let lower = 0;
            let upper = 3;
            let type_id = self
                .dwarf
                .unit
                .add(array_type_id, gimli::DW_TAG_subrange_type);
            let entry = self.dwarf.unit.get_mut(type_id);
            entry.set(
                gimli::DW_AT_type,
                gimli::write::AttributeValue::UnitRef(base_type),
            );
            entry.set(
                gimli::DW_AT_lower_bound,
                gimli::write::AttributeValue::Sdata(lower),
            );
            entry.set(
                gimli::DW_AT_upper_bound,
                gimli::write::AttributeValue::Sdata(upper),
            );
            type_id
        };

        let field_entry_id = self.dwarf.unit.add(type_id, gimli::DW_TAG_member);
        let field_entry = self.dwarf.unit.get_mut(field_entry_id);
        field_entry.set(
            gimli::DW_AT_name,
            gimli::write::AttributeValue::StringRef(self.dwarf.strings.add("unit")),
        );
        field_entry.set(
            gimli::DW_AT_type,
            gimli::write::AttributeValue::UnitRef(array_type_id),
        );
        field_entry.set(
            gimli::DW_AT_data_member_location,
            gimli::write::AttributeValue::Udata(0),
        );

        let struct_entry = self.dwarf.unit.get_mut(type_id);
        struct_entry.set(
            gimli::DW_AT_byte_size,
            gimli::write::AttributeValue::Udata(4 * 8),
        );

        type_id
    }

    fn set_type_sorted_array(&mut self) -> gimli::write::UnitEntryId {
        let type_id = self
            .dwarf
            .unit
            .add(self.dwarf.unit.root(), gimli::DW_TAG_structure_type);

        // We model this as a dynamic array that accesses its size, found 8 bytes before it.
        let base_type = self.basic_type_impl("int64_t", 8, gimli::DW_ATE_signed);
        let dynamic_array_type = {
            let type_id = self
                .dwarf
                .unit
                .add(self.dwarf.unit.root(), gimli::DW_TAG_array_type);

            let entry = self.dwarf.unit.get_mut(type_id);
            entry.set(
                gimli::DW_AT_type,
                gimli::write::AttributeValue::UnitRef(base_type),
            );
            entry.set(
                gimli::DW_AT_ordering,
                gimli::write::AttributeValue::Ordering(gimli::DW_ORD_row_major),
            );
            let mut expr = gimli::write::Expression::new();
            expr.op(gimli::constants::DW_OP_push_object_address);
            expr.op(gimli::constants::DW_OP_lit8);
            expr.op(gimli::constants::DW_OP_plus);
            expr.op(gimli::constants::DW_OP_deref);
            entry.set(
                gimli::DW_AT_data_location,
                gimli::write::AttributeValue::Exprloc(expr),
            );
            type_id
        };

        let _subrange_type = {
            let type_id = self
                .dwarf
                .unit
                .add(dynamic_array_type, gimli::DW_TAG_subrange_type);
            let entry = self.dwarf.unit.get_mut(type_id);
            entry.set(
                gimli::DW_AT_type,
                gimli::write::AttributeValue::UnitRef(base_type),
            );
            entry.set(
                gimli::DW_AT_lower_bound,
                gimli::write::AttributeValue::Sdata(0),
            );
            let mut expr = gimli::write::Expression::new();
            expr.op(gimli::constants::DW_OP_push_object_address);
            expr.op(gimli::constants::DW_OP_deref);
            entry.set(
                gimli::DW_AT_count,
                gimli::write::AttributeValue::Exprloc(expr),
            );
            type_id
        };
        let field_entry_id = self.dwarf.unit.add(type_id, gimli::DW_TAG_member);
        let field_entry = self.dwarf.unit.get_mut(field_entry_id);
        field_entry.set(
            gimli::DW_AT_name,
            gimli::write::AttributeValue::StringRef(self.dwarf.strings.add("values")),
        );
        field_entry.set(
            gimli::DW_AT_type,
            gimli::write::AttributeValue::UnitRef(dynamic_array_type),
        );
        field_entry.set(
            gimli::DW_AT_data_member_location,
            gimli::write::AttributeValue::Udata(0),
        );

        let struct_entry = self.dwarf.unit.get_mut(type_id);
        struct_entry.set(
            gimli::DW_AT_byte_size,
            gimli::write::AttributeValue::Udata(2 * 8),
        );

        type_id
    }

    fn set_type(
        &mut self,
        _semantic_context: &pasko_frontend::semantic::SemanticContext,
        _ty: pasko_frontend::typesystem::TypeId,
        _size_and_alignment_cache: &SizeAndAlignmentCache,
        _offset_cache: &OffsetCache,
    ) -> gimli::write::UnitEntryId {
        if let Some(set_type) = self.set_type {
            return set_type;
        }

        let set_struct_type_id = self
            .dwarf
            .unit
            .add(self.dwarf.unit.root(), gimli::DW_TAG_structure_type);

        let variant_part_id = self
            .dwarf
            .unit
            .add(set_struct_type_id, gimli::DW_TAG_variant_part);

        let discriminant = self.dwarf.unit.add(variant_part_id, gimli::DW_TAG_member);
        {
            let variant_part_entry = self.dwarf.unit.get_mut(variant_part_id);
            variant_part_entry.set(
                gimli::DW_AT_discr,
                gimli::write::AttributeValue::UnitRef(discriminant),
            );
        }
        {
            let enum_mode_type = self.set_type_enum_mode();
            let discriminant_entry = self.dwarf.unit.get_mut(discriminant);
            discriminant_entry.set(
                gimli::DW_AT_type,
                gimli::write::AttributeValue::UnitRef(enum_mode_type),
            );
            discriminant_entry.set(
                gimli::DW_AT_name,
                gimli::write::AttributeValue::StringRef(self.dwarf.strings.add("mode")),
            );
            discriminant_entry.set(
                gimli::DW_AT_data_member_location,
                gimli::write::AttributeValue::Udata(0),
            );
        }

        // First variant part. SM_BITMAP
        let variant_entry_id = self.dwarf.unit.add(variant_part_id, gimli::DW_TAG_variant);
        let variant_entry = self.dwarf.unit.get_mut(variant_entry_id);
        variant_entry.set(
            gimli::DW_AT_discr_value,
            gimli::write::AttributeValue::Udata(0),
        );
        let bitmap_type = self.set_type_bitmap();
        let field_entry_id = self.dwarf.unit.add(variant_entry_id, gimli::DW_TAG_member);
        let field_entry = self.dwarf.unit.get_mut(field_entry_id);
        field_entry.set(
            gimli::DW_AT_type,
            gimli::write::AttributeValue::UnitRef(bitmap_type),
        );
        field_entry.set(
            gimli::DW_AT_name,
            gimli::write::AttributeValue::StringRef(self.dwarf.strings.add("bitmap")),
        );
        field_entry.set(
            gimli::DW_AT_data_member_location,
            gimli::write::AttributeValue::Sdata(8),
        );

        // Second variant part. SM_SORTED_ARRAY
        let variant_entry_id = self.dwarf.unit.add(variant_part_id, gimli::DW_TAG_variant);
        let variant_entry = self.dwarf.unit.get_mut(variant_entry_id);
        variant_entry.set(
            gimli::DW_AT_discr_value,
            gimli::write::AttributeValue::Udata(1),
        );
        let sorted_array = self.set_type_sorted_array();
        let field_entry_id = self.dwarf.unit.add(variant_entry_id, gimli::DW_TAG_member);
        let field_entry = self.dwarf.unit.get_mut(field_entry_id);
        field_entry.set(
            gimli::DW_AT_type,
            gimli::write::AttributeValue::UnitRef(sorted_array),
        );
        field_entry.set(
            gimli::DW_AT_name,
            gimli::write::AttributeValue::StringRef(self.dwarf.strings.add("array")),
        );
        field_entry.set(
            gimli::DW_AT_data_member_location,
            gimli::write::AttributeValue::Sdata(8),
        );

        // Whole structure information.
        let entry = self.dwarf.unit.get_mut(set_struct_type_id);
        entry.set(
            gimli::DW_AT_byte_size,
            gimli::write::AttributeValue::Udata(4 + 4 + 8 * 4),
        );

        let type_id = {
            let pointer_type = self
                .dwarf
                .unit
                .add(self.dwarf.unit.root(), gimli::DW_TAG_reference_type);

            let entry = self.dwarf.unit.get_mut(pointer_type);
            entry.set(
                gimli::DW_AT_type,
                gimli::write::AttributeValue::UnitRef(set_struct_type_id),
            );
            entry.set(
                gimli::DW_AT_byte_size,
                gimli::write::AttributeValue::Udata(8),
            );
            pointer_type
        };

        self.set_type = Some(type_id);
        type_id
    }

    fn typedef_type(
        &mut self,
        semantic_context: &pasko_frontend::semantic::SemanticContext,
        ty: pasko_frontend::typesystem::TypeId,
        size_and_alignment_cache: &SizeAndAlignmentCache,
        offset_cache: &OffsetCache,
    ) -> gimli::write::UnitEntryId {
        let sym_id = semantic_context.type_system.named_type_get_symbol(ty);
        let sym = semantic_context.get_symbol(sym_id);
        let sym = sym.borrow();
        let sym_type = sym.get_type().unwrap();
        let base_type = self.debug_type(
            semantic_context,
            sym_type,
            size_and_alignment_cache,
            offset_cache,
        );

        let type_id = self
            .dwarf
            .unit
            .add(self.dwarf.unit.root(), gimli::DW_TAG_typedef);
        let entry = self.dwarf.unit.get_mut(type_id);
        entry.set(
            gimli::DW_AT_type,
            gimli::write::AttributeValue::UnitRef(base_type),
        );
        entry.set(
            gimli::DW_AT_name,
            gimli::write::AttributeValue::StringRef(
                self.dwarf.strings.add(sym.get_name().as_str()),
            ),
        );

        type_id
    }

    fn unspecified_type(
        &mut self,
        _semantic_context: &pasko_frontend::semantic::SemanticContext,
        _ty: pasko_frontend::typesystem::TypeId,
        _size_and_alignment_cache: &SizeAndAlignmentCache,
        _offset_cache: &OffsetCache,
    ) -> gimli::write::UnitEntryId {
        let type_id = self
            .dwarf
            .unit
            .add(self.dwarf.unit.root(), gimli::DW_TAG_unspecified_type);

        type_id
    }

    pub fn create_local_locations(
        &mut self,
        function_symbol_id: Option<pasko_frontend::symbol::SymbolId>,
        semantic_context: &pasko_frontend::semantic::SemanticContext,
        target_isa: &dyn cranelift_codegen::isa::TargetIsa,
        func_id: cranelift_module::FuncId,
        value_label_ranges: &HashMap<
            cranelift_codegen::ir::ValueLabel,
            Vec<cranelift_codegen::ValueLocRange>,
        >,
        linemap: &span::LineMap,
        size_and_alignment_cache: &SizeAndAlignmentCache,
        offset_cache: &OffsetCache,
    ) {
        let (function_entry_id, _) = if let Some(function_symbol_id) = function_symbol_id {
            self.get_function_entry_id(semantic_context, function_symbol_id)
        } else {
            self.get_main_function_entry_id()
        };
        let mut value_label_ranges: Vec<_> = value_label_ranges.iter().collect();
        value_label_ranges.sort_by(|label_a, label_b| {
            let sym_id_a = SymbolId::build_from_id(label_a.0.as_u32() as usize);
            let sym_a = semantic_context.get_symbol(sym_id_a);
            let sym_a = sym_a.borrow();
            let loc_a = sym_a.get_defining_point().unwrap();

            let sym_id_b = SymbolId::build_from_id(label_b.0.as_u32() as usize);
            let sym_b = semantic_context.get_symbol(sym_id_b);
            let sym_b = sym_b.borrow();
            let loc_b = sym_b.get_defining_point().unwrap();

            loc_a.begin().cmp(&loc_b.begin())
        });

        for (label, ranges) in value_label_ranges {
            let sym_id = SymbolId::build_from_id(label.as_u32() as usize);
            let sym = semantic_context.get_symbol(sym_id);
            let sym = sym.borrow();

            let (line, col) =
                linemap.offset_to_line_and_col(sym.get_defining_point().unwrap().begin());

            // Parameters?????
            let local_entry_id = if sym.get_parameter().is_some() {
                // This is a parameter.
                self.dwarf
                    .unit
                    .add(function_entry_id, gimli::DW_TAG_formal_parameter)
            } else {
                self.dwarf
                    .unit
                    .add(function_entry_id, gimli::DW_TAG_variable)
            };
            let type_id = self.debug_type(
                semantic_context,
                sym.get_type().unwrap(),
                size_and_alignment_cache,
                offset_cache,
            );

            let locations = ranges
                .iter()
                .map(|range| {
                    let data = match range.loc {
                        cranelift_codegen::LabelValueLoc::Reg(reg) => {
                            let dwarf_reg = target_isa.map_regalloc_reg_to_dwarf(reg).unwrap();
                            let mut expr = gimli::write::Expression::new();
                            gimli::write::Expression::op_reg(&mut expr, gimli::Register(dwarf_reg));
                            expr
                        }
                        _ => unimplemented!(),
                    };
                    gimli::write::Location::StartEnd {
                        begin: address_for_func_offset(func_id, range.start as i64),
                        end: address_for_func_offset(func_id, range.end as i64),
                        data,
                    }
                })
                .collect::<Vec<_>>();
            let location_list = gimli::write::LocationList(locations);
            let location_list_id = self.dwarf.unit.locations.add(location_list);

            let entry = self.dwarf.unit.get_mut(local_entry_id);
            entry.set(
                gimli::DW_AT_name,
                gimli::write::AttributeValue::StringRef(
                    self.dwarf.strings.add(sym.get_name().as_str()),
                ),
            );
            entry.set(
                gimli::DW_AT_decl_file,
                gimli::write::AttributeValue::FileIndex(Some(self.file_id)),
            );
            entry.set(
                gimli::DW_AT_decl_line,
                gimli::write::AttributeValue::Udata(line as u64),
            );
            entry.set(
                gimli::DW_AT_decl_column,
                gimli::write::AttributeValue::Udata(col as u64),
            );
            entry.set(
                gimli::DW_AT_type,
                gimli::write::AttributeValue::UnitRef(type_id),
            );

            entry.set(
                gimli::DW_AT_location,
                gimli::write::AttributeValue::LocationListRef(location_list_id),
            );
        }
    }
}

fn address_for_func(func_id: cranelift_module::FuncId) -> gimli::write::Address {
    let symbol = func_id.as_u32();
    assert!(symbol & 1 << 31 == 0);
    gimli::write::Address::Symbol {
        symbol: symbol as usize,
        addend: 0,
    }
}

fn address_for_func_offset(
    func_id: cranelift_module::FuncId,
    offset: i64,
) -> gimli::write::Address {
    let symbol = func_id.as_u32();
    assert!(symbol & 1 << 31 == 0);
    gimli::write::Address::Symbol {
        symbol: symbol as usize,
        addend: offset,
    }
}

fn address_for_data(data_id: cranelift_module::DataId) -> gimli::write::Address {
    let symbol = data_id.as_u32();
    assert!(symbol & 1 << 31 == 0);
    gimli::write::Address::Symbol {
        symbol: (symbol | 1 << 31) as usize,
        addend: 0,
    }
}
