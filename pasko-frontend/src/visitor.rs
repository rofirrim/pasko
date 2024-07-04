use crate::ast;
use crate::span;

use paste::paste;

// Read only visitors. (Currently unused)
macro_rules! define_visitor {
    ($class:ident) => {
        paste! {
          fn [<visit_pre_ $class:snake>](&self, _n: &ast::$class, span: &span::SpanLoc, id: span::SpanId) -> bool {
            self.unhandled_node_pre(stringify!($class), span, id);
            true
          }
          fn [<visit_post_ $class:snake>](&self, _n: &ast::$class, span: &span::SpanLoc, id: span::SpanId)  {
            self.unhandled_node_post(stringify!($class), span, id);
          }
        }
    };
}

macro_rules! define_visitor_leaf {
    ($class:ident) => {
        paste! {
          fn [<visit_ $class:snake>](&self, _n: &ast::$class, span: &span::SpanLoc, id: span::SpanId) {
            self.unhandled_node_leaf(stringify!($class), span, id);
          }
        }
    };
}

// Visitors that can mutate the visitor object but not the AST.
macro_rules! define_visitor_mut {
    ($class:ident) => {
        paste! {
          fn [<visit_pre_ $class:snake>](&mut self, _n: &ast::$class, span: &span::SpanLoc, id: span::SpanId) -> bool {
            self.unhandled_node_pre(stringify!($class), span, id);
            true
          }
          fn [<visit_post_ $class:snake>](&mut self, _n: &ast::$class, span: &span::SpanLoc, id: span::SpanId)  {
            self.unhandled_node_post(stringify!($class), span, id);
          }
        }
    };
}

macro_rules! define_visitor_leaf_mut {
    ($class:ident) => {
        paste! {
          fn [<visit_ $class:snake>](&mut self, _n: &ast::$class, span: &span::SpanLoc, id: span::SpanId) {
            self.unhandled_node_leaf(stringify!($class), span, id);
          }
        }
    };
}

// Visitors that can mutate the AST itself.
macro_rules! define_mutating_visitor {
    ($class:ident) => {
        paste! {
          fn [<visit_pre_ $class:snake>](&self, _n: &mut ast::$class, span: &span::SpanLoc, id: span::SpanId) -> bool {
            self.unhandled_node_pre(stringify!($class), span, id);
            true
          }
          fn [<visit_post_ $class:snake>](&self, _n: &mut ast::$class, span: &span::SpanLoc, id: span::SpanId)  {
            self.unhandled_node_post(stringify!($class), span, id);
          }
        }
    };
}

macro_rules! define_mutating_visitor_leaf {
    ($class:ident) => {
        paste! {
          fn [<visit_ $class:snake>](&self, _n: &mut ast::$class, span: &span::SpanLoc, id: span::SpanId) {
            self.unhandled_node_leaf(stringify!($class), span, id);
          }
        }
    };
}

// Visitors that can mutate both the visitor object and the AST.
macro_rules! define_mutating_visitor_mut {
    ($class:ident) => {
        paste! {
          fn [<visit_pre_ $class:snake>](&mut self, _n: &mut ast::$class, span: &span::SpanLoc, id: span::SpanId) -> bool {
            self.unhandled_node_pre(stringify!($class), span, id);
            true
          }
          fn [<visit_post_ $class:snake>](&mut self, _n: &mut ast::$class, span: &span::SpanLoc, id: span::SpanId)  {
            self.unhandled_node_post(stringify!($class), span, id);
          }
        }
    };
}

macro_rules! define_mutating_visitor_leaf_mut {
    ($class:ident) => {
        paste! {
          fn [<visit_ $class:snake>](&mut self, _n: &mut ast::$class, span: &span::SpanLoc, id: span::SpanId) {
            self.unhandled_node_leaf(stringify!($class), span, id);
          }
        }
    };
}

macro_rules! define_visitor_trait {
    ($trait_name:ident, $define_visitor:ident, $define_visitor_leaf:ident) => {
        pub trait $trait_name {
            fn unhandled_node_pre(&self, _class: &str, _span: &span::SpanLoc, _id: span::SpanId) {}
            fn unhandled_node_post(&self, _class: &str, _span: &span::SpanLoc, _id: span::SpanId) {}
            fn unhandled_node_leaf(&self, _class: &str, _span: &span::SpanLoc, _id: span::SpanId) {}
            $define_visitor!(Program);
            $define_visitor_leaf!(ProgramHeading);
            $define_visitor!(ProgramBlock);
            $define_visitor!(Block);
            $define_visitor_leaf!(LabelDeclarationPart);
            $define_visitor!(ConstantDefinitionPart);
            $define_visitor!(ConstantDefinition);
            $define_visitor!(Const);
            $define_visitor_leaf!(ConstInteger);
            $define_visitor_leaf!(ConstReal);
            $define_visitor_leaf!(ConstNamed);
            $define_visitor_leaf!(ConstMinusNamed);
            $define_visitor_leaf!(ConstStringLiteral);
            $define_visitor_leaf!(ConstNil);
            $define_visitor_leaf!(ConstBool);
            $define_visitor!(TypeDefinitionPart);
            $define_visitor!(TypeDefinition);
            $define_visitor_leaf!(TypeIdentifier);
            $define_visitor_leaf!(EnumeratedType);
            $define_visitor!(TypeDenoter);
            $define_visitor!(SubrangeType);
            $define_visitor!(ArrayType);
            $define_visitor!(RecordType);
            $define_visitor!(RecordSection);
            $define_visitor!(SetType);
            $define_visitor!(FileType);
            $define_visitor!(PointerType);
            $define_visitor!(VariableDeclarationPart);
            $define_visitor!(VariableDeclaration);
            $define_visitor!(ProcedureAndFunctionDeclarationPart);
            $define_visitor!(ProcedureAndFunctionDeclaration);
            $define_visitor!(ProcedureDeclaration);
            $define_visitor!(Procedure);
            $define_visitor!(ProcedureForward);
            $define_visitor!(ProcedureDefinition);
            $define_visitor!(FunctionDeclaration);
            $define_visitor!(Function);
            $define_visitor!(FunctionForward);
            $define_visitor!(FunctionDefinition);
            $define_visitor!(FunctionLateDefinition);
            $define_visitor!(FormalParameter);
            $define_visitor!(FormalParameterValue);
            $define_visitor!(FormalParameterVariable);
            $define_visitor!(FormalParameterProcedure);
            $define_visitor!(FormalParameterFunction);
            $define_visitor!(StatementPart);
            $define_visitor!(Stmt);
            $define_visitor!(StmtLabel);
            $define_visitor!(StmtAssignment);
            $define_visitor!(StmtProcedureCall);
            $define_visitor_leaf!(StmtGoto);
            $define_visitor!(StmtCompound);
            $define_visitor!(StmtRepeatUntil);
            $define_visitor!(StmtWhileDo);
            $define_visitor!(StmtFor);
            $define_visitor!(StmtWith);
            $define_visitor!(StmtIf);
            $define_visitor!(StmtCase);
            $define_visitor_leaf!(StmtEmpty);
            $define_visitor!(CaseListElement);
            $define_visitor!(Assig);
            $define_visitor_leaf!(AssigVariable);
            $define_visitor!(AssigArrayAccess);
            $define_visitor!(AssigFieldAccess);
            $define_visitor!(AssigPointerDeref);
            $define_visitor!(Expr);
            $define_visitor!(ExprConst);
            $define_visitor!(ExprSetLiteral);
            $define_visitor!(ExprRange);
            $define_visitor!(ExprFunctionCall);
            $define_visitor!(ExprVariable);
            $define_visitor!(ExprVariableReference);
            $define_visitor!(ExprParentheses);
            $define_visitor!(ExprUnOp);
            $define_visitor!(ExprBinOp);
            $define_visitor!(ExprWriteParameter);
            $define_visitor!(ExprConversion);
        }
    };
}

define_visitor_trait!(Visitor, define_visitor, define_visitor_leaf);
define_visitor_trait!(VisitorMut, define_visitor_mut, define_visitor_leaf_mut);

define_visitor_trait!(
    MutatingVisitor,
    define_mutating_visitor,
    define_mutating_visitor_leaf
);
define_visitor_trait!(
    MutatingVisitorMut,
    define_mutating_visitor_mut,
    define_mutating_visitor_leaf_mut
);

// Visitables. They do not mutate the AST.
pub trait Visitable {
    fn walk(&self, v: &dyn Visitor, span: &span::SpanLoc, id: span::SpanId);
    fn walk_mut(&self, v: &mut dyn VisitorMut, span: &span::SpanLoc, id: span::SpanId);
}

macro_rules! define_visitable_leaf {
    ($class:ident) => {
        impl Visitable for ast::$class {
            fn walk(&self, v: &dyn Visitor, span: &span::SpanLoc, id: span::SpanId) {
                paste! { v. [<visit_ $class:snake>](self, span, id); }
            }
            fn walk_mut(&self, v: &mut dyn VisitorMut, span: &span::SpanLoc, id: span::SpanId) {
                paste! { v. [<visit_ $class:snake>](self, span, id); }
            }
        }
    };
}

macro_rules! visit_child {
    ($self_:ident, $x:tt, $visitor:expr) => {
        $self_
            .$x
            .get()
            .walk($visitor, $self_.$x.loc(), $self_.$x.id());
    };
}

macro_rules! visit_child_mut {
    ($self_:ident, $x:tt, $visitor:expr) => {
        $self_
            .$x
            .get()
            .walk_mut($visitor, $self_.$x.loc(), $self_.$x.id());
    };
}

macro_rules! visit_optional_child {
    ($self_:ident, $x:tt, $visitor:expr) => {
        if let Some(w) = &$self_.$x {
            w.get().walk($visitor, w.loc(), w.id());
        }
    };
}

macro_rules! visit_optional_child_mut {
    ($self_:ident, $x:tt, $visitor:expr) => {
        if let Some(w) = &$self_.$x {
            w.get().walk_mut($visitor, w.loc(), w.id());
        }
    };
}

macro_rules! visit_vector_child {
    ($self_:ident, $x:tt, $visitor:expr) => {
        $self_
            .$x
            .iter()
            .for_each(|e| e.get().walk($visitor, e.loc(), e.id()));
    };
}

macro_rules! visit_vector_child_mut {
    ($self_:ident, $x:tt, $visitor:expr) => {
        $self_
            .$x
            .iter()
            .for_each(|e| e.get().walk_mut($visitor, e.loc(), e.id()));
    };
}

macro_rules! visit_optional_vector_child {
    ($self_:ident, $x:tt, $visitor:expr) => {
        if let Some(w) = &$self_.$x {
            w.iter()
                .for_each(|e| e.get().walk($visitor, e.loc(), e.id()));
        }
    };
}

macro_rules! visit_optional_vector_child_mut {
    ($self_:ident, $x:tt, $visitor:expr) => {
        if let Some(w) = &$self_.$x {
            w.iter()
                .for_each(|e| e.get().walk_mut($visitor, e.loc(), e.id()));
        }
    };
}

macro_rules! define_visit_method {
  ($class:ident, $self_:ident, $visitor:ident, $body:block) => {
    fn walk(&$self_, $visitor: &dyn Visitor, span: &span::SpanLoc, id: span::SpanId) {
      paste! {
        if $visitor. [<visit_pre_ $class:snake>]($self_, span, id) {
            $body
            $visitor. [<visit_post_ $class:snake>]($self_, span, id);
        }
      }
    }
  };
}

macro_rules! define_visit_method_mut {
  ($class:ident, $self_:ident, $visitor:ident, $body_mut:block) => {
    fn walk_mut(&$self_, $visitor: &mut dyn VisitorMut, span: &span::SpanLoc, id: span::SpanId) {
      paste! {
        if $visitor. [<visit_pre_ $class:snake>]($self_, span, id) {
            $body_mut
            $visitor. [<visit_post_ $class:snake>]($self_, span, id);
        }
      }
    }
  };
}

macro_rules! define_visitable {
    ($class:ident, $self_:ident, $visitor:ident, $body:block, $body_mut:block) => {
        impl Visitable for ast::$class {
            define_visit_method!($class, $self_, $visitor, $body);
            define_visit_method_mut!($class, $self_, $visitor, $body_mut);
        }
    };
}

macro_rules! define_visitable_enum {
  ($class:ident, { $($variant:path),+} ) => {
        impl Visitable for ast::$class {
            fn walk(&self, v: &dyn Visitor, span: &span::SpanLoc, id: span::SpanId) {
                paste! {
                    if v. [<visit_pre_ $class:snake>](self, span, id) {
                        match self {
                            $(
                            $variant(x) => { x.walk(v, span, id) },
                            )+
                        }
                        v. [<visit_post_ $class:snake>](self, span, id);
                    }
                }
            }
            fn walk_mut(&self, v: &mut dyn VisitorMut, span: &span::SpanLoc, id: span::SpanId) {
                paste! {
                    if v. [<visit_pre_ $class:snake>](self, span, id) {
                        match self {
                            $(
                            $variant(x) => { x.walk_mut(v, span, id) },
                            )+
                        }
                        v. [<visit_post_ $class:snake>](self, span, id);
                    }
                }
            }
        }
  };
}

// Mutating visitables, they can modify the AST at postorder.
pub trait MutatingVisitable {
    fn mutating_walk(&mut self, v: &dyn MutatingVisitor, span: &span::SpanLoc, id: span::SpanId);
    fn mutating_walk_mut(
        &mut self,
        v: &mut dyn MutatingVisitorMut,
        span: &span::SpanLoc,
        id: span::SpanId,
    );
}

macro_rules! define_mutating_visitable_leaf {
    ($class:ident) => {
        impl MutatingVisitable for ast::$class {
            fn mutating_walk(
                &mut self,
                v: &dyn MutatingVisitor,
                span: &span::SpanLoc,
                id: span::SpanId,
            ) {
                paste! { v. [<visit_ $class:snake>](self, span, id); }
            }
            fn mutating_walk_mut(
                &mut self,
                v: &mut dyn MutatingVisitorMut,
                span: &span::SpanLoc,
                id: span::SpanId,
            ) {
                paste! { v. [<visit_ $class:snake>](self, span, id); }
            }
        }
    };
}

macro_rules! mutating_visit_child {
    ($self_:ident, $x:tt, $visitor:expr) => {
        let loc = *$self_.$x.loc();
        let id = $self_.$x.id();
        $self_.$x.get_mut().mutating_walk($visitor, &loc, id);
    };
}

macro_rules! mutating_visit_child_mut {
    ($self_:ident, $x:tt, $visitor:expr) => {
        let loc = *$self_.$x.loc();
        let id = $self_.$x.id();
        $self_.$x.get_mut().mutating_walk_mut($visitor, &loc, id);
    };
}

macro_rules! mutating_visit_optional_child {
    ($self_:ident, $x:tt, $visitor:expr) => {
        if let Some(w) = $self_.$x.as_mut() {
            let loc = *w.loc();
            let id = w.id();
            w.get_mut().mutating_walk($visitor, &loc, id);
        }
    };
}

macro_rules! mutating_visit_optional_child_mut {
    ($self_:ident, $x:tt, $visitor:expr) => {
        if let Some(w) = $self_.$x.as_mut() {
            let loc = *w.loc();
            let id = w.id();
            w.get_mut().mutating_walk_mut($visitor, &loc, id);
        }
    };
}

macro_rules! mutating_visit_vector_child {
    ($self_:ident, $x:tt, $visitor:expr) => {
        $self_.$x.iter_mut().for_each(|e| {
            let loc = *e.loc();
            let id = e.id();
            e.get_mut().mutating_walk($visitor, &loc, id)
        });
    };
}

macro_rules! mutating_visit_vector_child_mut {
    ($self_:ident, $x:tt, $visitor:expr) => {
        $self_.$x.iter_mut().for_each(|e| {
            let loc = *e.loc();
            let id = e.id();
            e.get_mut().mutating_walk_mut($visitor, &loc, id)
        });
    };
}

macro_rules! mutating_visit_optional_vector_child {
    ($self_:ident, $x:tt, $visitor:expr) => {
        if let Some(w) = $self_.$x.as_mut() {
            w.iter_mut().for_each(|e| {
                let loc = *e.loc();
                let id = e.id();
                e.get_mut().mutating_walk($visitor, &loc, id)
            });
        }
    };
}

macro_rules! mutating_visit_optional_vector_child_mut {
    ($self_:ident, $x:tt, $visitor:expr) => {
        if let Some(w) = $self_.$x.as_mut() {
            w.iter_mut().for_each(|e| {
                let loc = *e.loc();
                let id = e.id();
                e.get_mut().mutating_walk_mut($visitor, &loc, id)
            });
        }
    };
}

macro_rules! define_mutating_visit_method {
  ($class:ident, $self_:ident, $visitor:ident, $body:block) => {
    fn mutating_walk(&mut $self_, $visitor: &dyn MutatingVisitor, span: &span::SpanLoc, id: span::SpanId) {
      paste! {
        if $visitor. [<visit_pre_ $class:snake>]($self_, span, id) {
            $body
            $visitor. [<visit_post_ $class:snake>]($self_, span, id);
        }
      }
    }
  };
}

macro_rules! define_mutating_visit_method_mut {
  ($class:ident, $self_:ident, $visitor:ident, $body_mut:block) => {
    fn mutating_walk_mut(&mut $self_, $visitor: &mut dyn MutatingVisitorMut, span: &span::SpanLoc, id: span::SpanId) {
      paste! {
        if $visitor. [<visit_pre_ $class:snake>]($self_, span, id) {
            $body_mut
            $visitor. [<visit_post_ $class:snake>]($self_, span, id);
        }
      }
    }
  };
}

macro_rules! define_mutating_visitable {
    ($class:ident, $self_:ident, $visitor:ident, $body:block, $body_mut:block) => {
        impl MutatingVisitable for ast::$class {
            define_mutating_visit_method!($class, $self_, $visitor, $body);
            define_mutating_visit_method_mut!($class, $self_, $visitor, $body_mut);
        }
    };
}

macro_rules! define_mutating_visitable_enum {
  ($class:ident, { $($variant:path),+} ) => {
        impl MutatingVisitable for ast::$class {
            fn mutating_walk(&mut self, v: &dyn MutatingVisitor, span: &span::SpanLoc, id: span::SpanId) {
                paste! {
                    if v. [<visit_pre_ $class:snake>](self, span, id) {
                        match self {
                            $(
                            $variant(x) => { x.mutating_walk(v, span, id) },
                            )+
                        }
                        v. [<visit_post_ $class:snake>](self, span, id);
                    }
                }
            }
            fn mutating_walk_mut(&mut self, v: &mut dyn MutatingVisitorMut, span: &span::SpanLoc, id: span::SpanId) {
                paste! {
                    if v. [<visit_pre_ $class:snake>](self, span, id) {
                        match self {
                            $(
                            $variant(x) => { x.mutating_walk_mut(v, span, id) },
                            )+
                        }
                        v. [<visit_post_ $class:snake>](self, span, id);
                    }
                }
            }
        }
  };
}

/* Visitables */

// Program
define_visitable!(
    Program,
    self,
    v,
    {
        visit_child!(self, 0, v);
        visit_child!(self, 1, v);
    },
    {
        visit_child_mut!(self, 0, v);
        visit_child_mut!(self, 1, v);
    }
);

define_mutating_visitable!(
    Program,
    self,
    v,
    {
        mutating_visit_child!(self, 0, v);
        mutating_visit_child!(self, 1, v);
    },
    {
        mutating_visit_child_mut!(self, 0, v);
        mutating_visit_child_mut!(self, 1, v);
    }
);

// ProgramHeading
define_visitable_leaf!(ProgramHeading);
define_mutating_visitable_leaf!(ProgramHeading);

// ProgramBlock
define_visitable!(
    ProgramBlock,
    self,
    v,
    {
        visit_child!(self, 0, v);
    },
    {
        visit_child_mut!(self, 0, v);
    }
);

define_mutating_visitable!(
    ProgramBlock,
    self,
    v,
    {
        mutating_visit_child!(self, 0, v);
    },
    {
        mutating_visit_child_mut!(self, 0, v);
    }
);

// Block
define_visitable!(
    Block,
    self,
    v,
    {
        visit_optional_child!(self, 0, v);
        visit_optional_child!(self, 1, v);
        visit_optional_child!(self, 2, v);
        visit_optional_child!(self, 3, v);
        visit_optional_child!(self, 4, v);
        visit_child!(self, 5, v);
    },
    {
        visit_optional_child_mut!(self, 0, v);
        visit_optional_child_mut!(self, 1, v);
        visit_optional_child_mut!(self, 2, v);
        visit_optional_child_mut!(self, 3, v);
        visit_optional_child_mut!(self, 4, v);
        visit_child_mut!(self, 5, v);
    }
);

define_mutating_visitable!(
    Block,
    self,
    v,
    {
        mutating_visit_optional_child!(self, 0, v);
        mutating_visit_optional_child!(self, 1, v);
        mutating_visit_optional_child!(self, 2, v);
        mutating_visit_optional_child!(self, 3, v);
        mutating_visit_optional_child!(self, 4, v);
        mutating_visit_child!(self, 5, v);
    },
    {
        mutating_visit_optional_child_mut!(self, 0, v);
        mutating_visit_optional_child_mut!(self, 1, v);
        mutating_visit_optional_child_mut!(self, 2, v);
        mutating_visit_optional_child_mut!(self, 3, v);
        mutating_visit_optional_child_mut!(self, 4, v);
        mutating_visit_child_mut!(self, 5, v);
    }
);

// LabelDeclarationPart
define_visitable_leaf!(LabelDeclarationPart);
define_mutating_visitable_leaf!(LabelDeclarationPart);

// ConstantDefinitionPart
define_visitable!(
    ConstantDefinitionPart,
    self,
    v,
    {
        visit_vector_child!(self, 0, v);
    },
    {
        visit_vector_child_mut!(self, 0, v);
    }
);

define_mutating_visitable!(
    ConstantDefinitionPart,
    self,
    v,
    {
        mutating_visit_vector_child!(self, 0, v);
    },
    {
        mutating_visit_vector_child_mut!(self, 0, v);
    }
);

// ConstantDefinition
define_visitable!(
    ConstantDefinition,
    self,
    v,
    {
        visit_child!(self, 1, v);
    },
    {
        visit_child_mut!(self, 1, v);
    }
);

define_mutating_visitable!(
    ConstantDefinition,
    self,
    v,
    {
        mutating_visit_child!(self, 1, v);
    },
    {
        mutating_visit_child_mut!(self, 1, v);
    }
);

// Const
define_visitable_enum!(
    Const,
    {
      ast::Const::Integer,
      ast::Const::Real,
      ast::Const::Named,
      ast::Const::MinusNamed,
      ast::Const::StringLiteral,
      ast::Const::Nil,
      ast::Const::Bool
    }
);

define_mutating_visitable_enum!(
    Const,
    {
      ast::Const::Integer,
      ast::Const::Real,
      ast::Const::Named,
      ast::Const::MinusNamed,
      ast::Const::StringLiteral,
      ast::Const::Nil,
      ast::Const::Bool
    }
);

// Leaf consts.
define_visitable_leaf!(ConstInteger);
define_mutating_visitable_leaf!(ConstInteger);

define_visitable_leaf!(ConstReal);
define_mutating_visitable_leaf!(ConstReal);

define_visitable_leaf!(ConstNamed);
define_mutating_visitable_leaf!(ConstNamed);

define_visitable_leaf!(ConstMinusNamed);
define_mutating_visitable_leaf!(ConstMinusNamed);

define_visitable_leaf!(ConstStringLiteral);
define_mutating_visitable_leaf!(ConstStringLiteral);

define_visitable_leaf!(ConstNil);
define_mutating_visitable_leaf!(ConstNil);

define_visitable_leaf!(ConstBool);
define_mutating_visitable_leaf!(ConstBool);

// TypeDefinitionPart
define_visitable!(
    TypeDefinitionPart,
    self,
    v,
    {
        visit_vector_child!(self, 0, v);
    },
    {
        visit_vector_child_mut!(self, 0, v);
    }
);

define_mutating_visitable!(
    TypeDefinitionPart,
    self,
    v,
    {
        mutating_visit_vector_child!(self, 0, v);
    },
    {
        mutating_visit_vector_child_mut!(self, 0, v);
    }
);

// TypeDefinition
define_visitable!(
    TypeDefinition,
    self,
    v,
    {
        visit_child!(self, 1, v);
    },
    {
        visit_child_mut!(self, 1, v);
    }
);

define_mutating_visitable!(
    TypeDefinition,
    self,
    v,
    {
        mutating_visit_child!(self, 1, v);
    },
    {
        mutating_visit_child_mut!(self, 1, v);
    }
);

// TypeDenoter
define_visitable_enum!(
  TypeDenoter,
  {
    ast::TypeDenoter::TypeIdentifier,
    ast::TypeDenoter::EnumeratedType,
    ast::TypeDenoter::SubrangeType,
    ast::TypeDenoter::ArrayType,
    ast::TypeDenoter::RecordType,
    ast::TypeDenoter::SetType,
    ast::TypeDenoter::FileType,
    ast::TypeDenoter::PointerType
  }
);

define_mutating_visitable_enum!(
  TypeDenoter,
  {
    ast::TypeDenoter::TypeIdentifier,
    ast::TypeDenoter::EnumeratedType,
    ast::TypeDenoter::SubrangeType,
    ast::TypeDenoter::ArrayType,
    ast::TypeDenoter::RecordType,
    ast::TypeDenoter::SetType,
    ast::TypeDenoter::FileType,
    ast::TypeDenoter::PointerType
  }
);

// TypeIdentifier
define_visitable_leaf!(TypeIdentifier);
define_mutating_visitable_leaf!(TypeIdentifier);

// EnumeratedType
define_visitable_leaf!(EnumeratedType);
define_mutating_visitable_leaf!(EnumeratedType);

// SubrangeType
define_visitable!(
    SubrangeType,
    self,
    v,
    {
        visit_child!(self, 0, v);
        visit_child!(self, 1, v);
    },
    {
        visit_child_mut!(self, 0, v);
        visit_child_mut!(self, 1, v);
    }
);

define_mutating_visitable!(
    SubrangeType,
    self,
    v,
    {
        mutating_visit_child!(self, 0, v);
        mutating_visit_child!(self, 1, v);
    },
    {
        mutating_visit_child_mut!(self, 0, v);
        mutating_visit_child_mut!(self, 1, v);
    }
);

// ArrayType
define_visitable!(
    ArrayType,
    self,
    v,
    {
        visit_vector_child!(self, 1, v);
        visit_child!(self, 2, v);
    },
    {
        visit_vector_child_mut!(self, 1, v);
        visit_child_mut!(self, 2, v);
    }
);

define_mutating_visitable!(
    ArrayType,
    self,
    v,
    {
        mutating_visit_vector_child!(self, 1, v);
        mutating_visit_child!(self, 2, v);
    },
    {
        mutating_visit_vector_child_mut!(self, 1, v);
        mutating_visit_child_mut!(self, 2, v);
    }
);

// SetType
define_visitable!(
    SetType,
    self,
    v,
    {
        visit_child!(self, 1, v);
    },
    {
        visit_child_mut!(self, 1, v);
    }
);

define_mutating_visitable!(
    SetType,
    self,
    v,
    {
        mutating_visit_child!(self, 1, v);
    },
    {
        mutating_visit_child_mut!(self, 1, v);
    }
);

// FileType
define_visitable!(
    FileType,
    self,
    v,
    {
        visit_child!(self, 1, v);
    },
    {
        visit_child_mut!(self, 1, v);
    }
);

define_mutating_visitable!(
    FileType,
    self,
    v,
    {
        mutating_visit_child!(self, 1, v);
    },
    {
        mutating_visit_child_mut!(self, 1, v);
    }
);

// PointerType
define_visitable!(
    PointerType,
    self,
    v,
    {
        visit_child!(self, 0, v);
    },
    {
        visit_child_mut!(self, 0, v);
    }
);

define_mutating_visitable!(
    PointerType,
    self,
    v,
    {
        mutating_visit_child!(self, 0, v);
    },
    {
        mutating_visit_child_mut!(self, 0, v);
    }
);

// RecordType
define_visitable!(
    RecordType,
    self,
    v,
    {
        visit_vector_child!(self, 1, v);
    },
    {
        visit_vector_child_mut!(self, 1, v);
    }
);

define_mutating_visitable!(
    RecordType,
    self,
    v,
    {
        mutating_visit_vector_child!(self, 1, v);
    },
    {
        mutating_visit_vector_child_mut!(self, 1, v);
    }
);

// RecordSection
define_visitable!(
    RecordSection,
    self,
    v,
    {
        visit_child!(self, 1, v);
    },
    {
        visit_child_mut!(self, 1, v);
    }
);

define_mutating_visitable!(
    RecordSection,
    self,
    v,
    {
        mutating_visit_child!(self, 1, v);
    },
    {
        mutating_visit_child_mut!(self, 1, v);
    }
);

// VariableDeclarationPart
define_visitable!(
    VariableDeclarationPart,
    self,
    v,
    {
        visit_vector_child!(self, 0, v);
    },
    {
        visit_vector_child_mut!(self, 0, v);
    }
);

define_mutating_visitable!(
    VariableDeclarationPart,
    self,
    v,
    {
        mutating_visit_vector_child!(self, 0, v);
    },
    {
        mutating_visit_vector_child_mut!(self, 0, v);
    }
);

// VariableDeclaration
define_visitable!(
    VariableDeclaration,
    self,
    v,
    {
        visit_child!(self, 1, v);
    },
    {
        visit_child_mut!(self, 1, v);
    }
);

define_mutating_visitable!(
    VariableDeclaration,
    self,
    v,
    {
        mutating_visit_child!(self, 1, v);
    },
    {
        mutating_visit_child_mut!(self, 1, v);
    }
);

// ProcedureAndFunctionDeclarationPart
define_visitable!(
    ProcedureAndFunctionDeclarationPart,
    self,
    v,
    {
        visit_vector_child!(self, 0, v);
    },
    {
        visit_vector_child_mut!(self, 0, v);
    }
);
define_mutating_visitable!(
    ProcedureAndFunctionDeclarationPart,
    self,
    v,
    {
        mutating_visit_vector_child!(self, 0, v);
    },
    {
        mutating_visit_vector_child_mut!(self, 0, v);
    }
);

// ProcedureAndFunctionDeclaration
define_visitable_enum!(ProcedureAndFunctionDeclaration,
{
    ast::ProcedureAndFunctionDeclaration::Procedure,
    ast::ProcedureAndFunctionDeclaration::Function
}
);

define_mutating_visitable_enum!(ProcedureAndFunctionDeclaration,
{
    ast::ProcedureAndFunctionDeclaration::Procedure,
    ast::ProcedureAndFunctionDeclaration::Function
}
);

// Procedure.
define_visitable!(
    Procedure,
    self,
    v,
    {
        visit_child!(self, 0, v);
    },
    {
        visit_child_mut!(self, 0, v);
    }
);

define_mutating_visitable!(
    Procedure,
    self,
    v,
    {
        mutating_visit_child!(self, 0, v);
    },
    {
        mutating_visit_child_mut!(self, 0, v);
    }
);

// ProcedureDeclaration
define_visitable_enum!(ProcedureDeclaration,
{
    ast::ProcedureDeclaration::Forward,
    ast::ProcedureDeclaration::Definition
}
);

define_mutating_visitable_enum!(ProcedureDeclaration,
{
    ast::ProcedureDeclaration::Forward,
    ast::ProcedureDeclaration::Definition
}
);

// ProcedureForward
define_visitable!(
    ProcedureForward,
    self,
    v,
    {
        visit_optional_vector_child!(self, 1, v);
    },
    {
        visit_optional_vector_child_mut!(self, 1, v);
    }
);

define_mutating_visitable!(
    ProcedureForward,
    self,
    v,
    {
        mutating_visit_optional_vector_child!(self, 1, v);
    },
    {
        mutating_visit_optional_vector_child_mut!(self, 1, v);
    }
);

// ProcedureDefinition
define_visitable!(
    ProcedureDefinition,
    self,
    v,
    {
        visit_optional_vector_child!(self, 1, v);
        visit_child!(self, 2, v);
    },
    {
        visit_optional_vector_child_mut!(self, 1, v);
        visit_child_mut!(self, 2, v);
    }
);

define_mutating_visitable!(
    ProcedureDefinition,
    self,
    v,
    {
        mutating_visit_optional_vector_child!(self, 1, v);
        mutating_visit_child!(self, 2, v);
    },
    {
        mutating_visit_optional_vector_child_mut!(self, 1, v);
        mutating_visit_child_mut!(self, 2, v);
    }
);

// Function
define_visitable!(
    Function,
    self,
    v,
    {
        visit_child!(self, 0, v);
    },
    {
        visit_child_mut!(self, 0, v);
    }
);

define_mutating_visitable!(
    Function,
    self,
    v,
    {
        mutating_visit_child!(self, 0, v);
    },
    {
        mutating_visit_child_mut!(self, 0, v);
    }
);

// FunctionDeclaration
define_visitable_enum!(FunctionDeclaration,
    {
        ast::FunctionDeclaration::Forward,
        ast::FunctionDeclaration::Definition,
        ast::FunctionDeclaration::LateDefinition
    }
);

define_mutating_visitable_enum!(FunctionDeclaration,
    {
        ast::FunctionDeclaration::Forward,
        ast::FunctionDeclaration::Definition,
        ast::FunctionDeclaration::LateDefinition
    }
);

// FunctionForward
define_visitable!(
    FunctionForward,
    self,
    v,
    {
        visit_optional_vector_child!(self, 1, v);
        visit_child!(self, 2, v);
    },
    {
        visit_optional_vector_child_mut!(self, 1, v);
        visit_child_mut!(self, 2, v);
    }
);

define_mutating_visitable!(
    FunctionForward,
    self,
    v,
    {
        mutating_visit_optional_vector_child!(self, 1, v);
        mutating_visit_child!(self, 2, v);
    },
    {
        mutating_visit_optional_vector_child_mut!(self, 1, v);
        mutating_visit_child_mut!(self, 2, v);
    }
);

// FunctionDefinition
define_visitable!(
    FunctionDefinition,
    self,
    v,
    {
        visit_optional_vector_child!(self, 1, v);
        visit_child!(self, 2, v);
        visit_child!(self, 3, v);
    },
    {
        visit_optional_vector_child_mut!(self, 1, v);
        visit_child_mut!(self, 2, v);
        visit_child_mut!(self, 3, v);
    }
);

define_mutating_visitable!(
    FunctionDefinition,
    self,
    v,
    {
        mutating_visit_optional_vector_child!(self, 1, v);
        mutating_visit_child!(self, 2, v);
        mutating_visit_child!(self, 3, v);
    },
    {
        mutating_visit_optional_vector_child_mut!(self, 1, v);
        mutating_visit_child_mut!(self, 2, v);
        mutating_visit_child_mut!(self, 3, v);
    }
);

// FunctionLateDefinition
define_visitable!(
    FunctionLateDefinition,
    self,
    v,
    {
        visit_child!(self, 1, v);
    },
    {
        visit_child_mut!(self, 1, v);
    }
);

define_mutating_visitable!(
    FunctionLateDefinition,
    self,
    v,
    {
        mutating_visit_child!(self, 1, v);
    },
    {
        mutating_visit_child_mut!(self, 1, v);
    }
);

// FormalParameter
define_visitable_enum!(FormalParameter, {
    ast::FormalParameter::Value,
    ast::FormalParameter::Variable,
    ast::FormalParameter::Procedure,
    ast::FormalParameter::Function
});

define_mutating_visitable_enum!(FormalParameter, {
    ast::FormalParameter::Value,
    ast::FormalParameter::Variable,
    ast::FormalParameter::Procedure,
    ast::FormalParameter::Function
});

// FormatlParameterValue
define_visitable!(
    FormalParameterValue,
    self,
    v,
    {
        visit_child!(self, 1, v);
    },
    {
        visit_child_mut!(self, 1, v);
    }
);
define_mutating_visitable!(
    FormalParameterValue,
    self,
    v,
    {
        mutating_visit_child!(self, 1, v);
    },
    {
        mutating_visit_child_mut!(self, 1, v);
    }
);

// FormalParameterVariable
define_visitable!(
    FormalParameterVariable,
    self,
    v,
    {
        visit_child!(self, 1, v);
    },
    {
        visit_child_mut!(self, 1, v);
    }
);
define_mutating_visitable!(
    FormalParameterVariable,
    self,
    v,
    {
        mutating_visit_child!(self, 1, v);
    },
    {
        mutating_visit_child_mut!(self, 1, v);
    }
);

// FormalParameterProcedure
define_visitable!(
    FormalParameterProcedure,
    self,
    v,
    {
        visit_optional_vector_child!(self, 1, v);
    },
    {
        visit_optional_vector_child_mut!(self, 1, v);
    }
);
define_mutating_visitable!(
    FormalParameterProcedure,
    self,
    v,
    {
        mutating_visit_optional_vector_child!(self, 1, v);
    },
    {
        mutating_visit_optional_vector_child_mut!(self, 1, v);
    }
);

// FormalParameterFunction
define_visitable!(
    FormalParameterFunction,
    self,
    v,
    {
        visit_optional_vector_child!(self, 1, v);
        visit_child!(self, 2, v);
    },
    {
        visit_optional_vector_child_mut!(self, 1, v);
        visit_child_mut!(self, 2, v);
    }
);

define_mutating_visitable!(
    FormalParameterFunction,
    self,
    v,
    {
        mutating_visit_optional_vector_child!(self, 1, v);
        mutating_visit_child!(self, 2, v);
    },
    {
        mutating_visit_optional_vector_child_mut!(self, 1, v);
        mutating_visit_child_mut!(self, 2, v);
    }
);

// StatementPart
define_visitable!(
    StatementPart,
    self,
    v,
    {
        visit_child!(self, 0, v);
    },
    {
        visit_child_mut!(self, 0, v);
    }
);

define_mutating_visitable!(
    StatementPart,
    self,
    v,
    {
        mutating_visit_child!(self, 0, v);
    },
    {
        mutating_visit_child_mut!(self, 0, v);
    }
);

// Stmt
define_visitable_enum!(Stmt,
{
    ast::Stmt::Label,
    ast::Stmt::Assignment,
    ast::Stmt::ProcedureCall,
    ast::Stmt::Goto,
    ast::Stmt::Compound,
    ast::Stmt::RepeatUntil,
    ast::Stmt::WhileDo,
    ast::Stmt::For,
    ast::Stmt::With,
    ast::Stmt::If,
    ast::Stmt::Case,
    ast::Stmt::Empty
});

define_mutating_visitable_enum!(Stmt,
{
    ast::Stmt::Label,
    ast::Stmt::Assignment,
    ast::Stmt::ProcedureCall,
    ast::Stmt::Goto,
    ast::Stmt::Compound,
    ast::Stmt::RepeatUntil,
    ast::Stmt::WhileDo,
    ast::Stmt::For,
    ast::Stmt::With,
    ast::Stmt::If,
    ast::Stmt::Case,
    ast::Stmt::Empty
});

// StmtLabel
define_visitable!(
    StmtLabel,
    self,
    v,
    {
        visit_child!(self, 1, v);
    },
    {
        visit_child_mut!(self, 1, v);
    }
);

define_mutating_visitable!(
    StmtLabel,
    self,
    v,
    {
        mutating_visit_child!(self, 1, v);
    },
    {
        mutating_visit_child_mut!(self, 1, v);
    }
);

// StmtAssignment
define_visitable!(
    StmtAssignment,
    self,
    v,
    {
        visit_child!(self, 0, v);
        visit_child!(self, 1, v);
    },
    {
        visit_child_mut!(self, 0, v);
        visit_child_mut!(self, 1, v);
    }
);

define_mutating_visitable!(
    StmtAssignment,
    self,
    v,
    {
        mutating_visit_child!(self, 0, v);
        mutating_visit_child!(self, 1, v);
    },
    {
        mutating_visit_child_mut!(self, 0, v);
        mutating_visit_child_mut!(self, 1, v);
    }
);

// StmtProcedureCall
define_visitable!(
    StmtProcedureCall,
    self,
    v,
    {
        visit_optional_vector_child!(self, 1, v);
    },
    {
        visit_optional_vector_child_mut!(self, 1, v);
    }
);

define_mutating_visitable!(
    StmtProcedureCall,
    self,
    v,
    {
        mutating_visit_optional_vector_child!(self, 1, v);
    },
    {
        mutating_visit_optional_vector_child_mut!(self, 1, v);
    }
);

// StmtGoto
define_visitable_leaf!(StmtGoto);
define_mutating_visitable_leaf!(StmtGoto);

// StmtCompound
define_visitable!(
    StmtCompound,
    self,
    v,
    {
        visit_vector_child!(self, 0, v);
    },
    {
        visit_vector_child_mut!(self, 0, v);
    }
);

define_mutating_visitable!(
    StmtCompound,
    self,
    v,
    {
        mutating_visit_vector_child!(self, 0, v);
    },
    {
        mutating_visit_vector_child_mut!(self, 0, v);
    }
);

// StmtRepeatUntil
define_visitable!(
    StmtRepeatUntil,
    self,
    v,
    {
        visit_vector_child!(self, 0, v);
        visit_child!(self, 1, v);
    },
    {
        visit_vector_child_mut!(self, 0, v);
        visit_child_mut!(self, 1, v);
    }
);

define_mutating_visitable!(
    StmtRepeatUntil,
    self,
    v,
    {
        mutating_visit_vector_child!(self, 0, v);
        mutating_visit_child!(self, 1, v);
    },
    {
        mutating_visit_vector_child_mut!(self, 0, v);
        mutating_visit_child_mut!(self, 1, v);
    }
);

// StmtWhileDo
define_visitable!(
    StmtWhileDo,
    self,
    v,
    {
        visit_child!(self, 0, v);
        visit_child!(self, 1, v);
    },
    {
        visit_child_mut!(self, 0, v);
        visit_child_mut!(self, 1, v);
    }
);

define_mutating_visitable!(
    StmtWhileDo,
    self,
    v,
    {
        mutating_visit_child!(self, 0, v);
        mutating_visit_child!(self, 1, v);
    },
    {
        mutating_visit_child_mut!(self, 0, v);
        mutating_visit_child_mut!(self, 1, v);
    }
);

// StmtFor
define_visitable!(
    StmtFor,
    self,
    v,
    {
        visit_child!(self, 1, v);
        visit_child!(self, 2, v);
        visit_child!(self, 3, v);
        visit_child!(self, 4, v);
    },
    {
        visit_child_mut!(self, 1, v);
        visit_child_mut!(self, 2, v);
        visit_child_mut!(self, 3, v);
        visit_child_mut!(self, 4, v);
    }
);

define_mutating_visitable!(
    StmtFor,
    self,
    v,
    {
        mutating_visit_child!(self, 1, v);
        mutating_visit_child!(self, 2, v);
        mutating_visit_child!(self, 3, v);
        mutating_visit_child!(self, 4, v);
    },
    {
        mutating_visit_child_mut!(self, 1, v);
        mutating_visit_child_mut!(self, 2, v);
        mutating_visit_child_mut!(self, 3, v);
        mutating_visit_child_mut!(self, 4, v);
    }
);

// StmtWith
define_visitable!(
    StmtWith,
    self,
    v,
    {
        visit_child!(self, 1, v);
    },
    {
        visit_child_mut!(self, 1, v);
    }
);

define_mutating_visitable!(
    StmtWith,
    self,
    v,
    {
        mutating_visit_child!(self, 1, v);
    },
    {
        mutating_visit_child_mut!(self, 1, v);
    }
);

// StmtIf
define_visitable!(
    StmtIf,
    self,
    v,
    {
        visit_child!(self, 0, v);
        visit_child!(self, 1, v);
        visit_optional_child!(self, 2, v);
    },
    {
        visit_child_mut!(self, 0, v);
        visit_child_mut!(self, 1, v);
        visit_optional_child_mut!(self, 2, v);
    }
);

define_mutating_visitable!(
    StmtIf,
    self,
    v,
    {
        mutating_visit_child!(self, 0, v);
        mutating_visit_child!(self, 1, v);
        mutating_visit_optional_child!(self, 2, v);
    },
    {
        mutating_visit_child_mut!(self, 0, v);
        mutating_visit_child_mut!(self, 1, v);
        mutating_visit_optional_child_mut!(self, 2, v);
    }
);

// StmtCase
define_visitable!(
    StmtCase,
    self,
    v,
    {
        visit_child!(self, 0, v);
        visit_vector_child!(self, 1, v);
    },
    {
        visit_child_mut!(self, 0, v);
        visit_vector_child_mut!(self, 1, v);
    }
);

define_mutating_visitable!(
    StmtCase,
    self,
    v,
    {
        mutating_visit_child!(self, 0, v);
        mutating_visit_vector_child!(self, 1, v);
    },
    {
        mutating_visit_child_mut!(self, 0, v);
        mutating_visit_vector_child_mut!(self, 1, v);
    }
);

// CaseListElement
define_visitable!(
    CaseListElement,
    self,
    v,
    {
        visit_vector_child!(self, 0, v);
        visit_child!(self, 1, v);
    },
    {
        visit_vector_child_mut!(self, 0, v);
        visit_child_mut!(self, 1, v);
    }
);

define_mutating_visitable!(
    CaseListElement,
    self,
    v,
    {
        mutating_visit_vector_child!(self, 0, v);
        mutating_visit_child!(self, 1, v);
    },
    {
        mutating_visit_vector_child_mut!(self, 0, v);
        mutating_visit_child_mut!(self, 1, v);
    }
);

// StmtEmpty
define_visitable_leaf!(StmtEmpty);
define_mutating_visitable_leaf!(StmtEmpty);

// Assig
define_visitable_enum!(Assig,
{
    ast::Assig::Variable,
    ast::Assig::ArrayAccess,
    ast::Assig::FieldAccess,
    ast::Assig::PointerDeref
});

define_mutating_visitable_enum!(Assig,
{
    ast::Assig::Variable,
    ast::Assig::ArrayAccess,
    ast::Assig::FieldAccess,
    ast::Assig::PointerDeref
});

// AssigVariable
define_visitable_leaf!(AssigVariable);
define_mutating_visitable_leaf!(AssigVariable);

// AssigArrayAccess
define_visitable!(
    AssigArrayAccess,
    self,
    v,
    {
        visit_child!(self, 0, v);
        visit_vector_child!(self, 1, v);
    },
    {
        visit_child_mut!(self, 0, v);
        visit_vector_child_mut!(self, 1, v);
    }
);

define_mutating_visitable!(
    AssigArrayAccess,
    self,
    v,
    {
        mutating_visit_child!(self, 0, v);
        mutating_visit_vector_child!(self, 1, v);
    },
    {
        mutating_visit_child_mut!(self, 0, v);
        mutating_visit_vector_child_mut!(self, 1, v);
    }
);

// AssigFieldAccess
define_visitable!(
    AssigFieldAccess,
    self,
    v,
    {
        visit_child!(self, 0, v);
    },
    {
        visit_child_mut!(self, 0, v);
    }
);

define_mutating_visitable!(
    AssigFieldAccess,
    self,
    v,
    {
        mutating_visit_child!(self, 0, v);
    },
    {
        mutating_visit_child_mut!(self, 0, v);
    }
);

// AssigPointerDeref
define_visitable!(
    AssigPointerDeref,
    self,
    v,
    {
        visit_child!(self, 0, v);
    },
    {
        visit_child_mut!(self, 0, v);
    }
);

define_mutating_visitable!(
    AssigPointerDeref,
    self,
    v,
    {
        mutating_visit_child!(self, 0, v);
    },
    {
        mutating_visit_child_mut!(self, 0, v);
    }
);

// Expr
define_visitable_enum!(Expr, {
    ast::Expr::Const,
    ast::Expr::SetLiteral,
    ast::Expr::Range,
    ast::Expr::FunctionCall,
    ast::Expr::Variable,
    ast::Expr::VariableReference,
    ast::Expr::Parentheses,
    ast::Expr::UnOp,
    ast::Expr::BinOp,
    ast::Expr::WriteParameter,
    ast::Expr::Conversion
});

define_mutating_visitable_enum!(Expr, {
    ast::Expr::Const,
    ast::Expr::SetLiteral,
    ast::Expr::Range,
    ast::Expr::FunctionCall,
    ast::Expr::Variable,
    ast::Expr::VariableReference,
    ast::Expr::Parentheses,
    ast::Expr::UnOp,
    ast::Expr::BinOp,
    ast::Expr::WriteParameter,
    ast::Expr::Conversion
});

// ExprConst
define_visitable!(
    ExprConst,
    self,
    v,
    {
        visit_child!(self, 0, v);
    },
    {
        visit_child_mut!(self, 0, v);
    }
);

define_mutating_visitable!(
    ExprConst,
    self,
    v,
    {
        mutating_visit_child!(self, 0, v);
    },
    {
        mutating_visit_child_mut!(self, 0, v);
    }
);

// ExprSetLiteral
define_visitable!(
    ExprSetLiteral,
    self,
    v,
    {
        visit_vector_child!(self, 0, v);
    },
    {
        visit_vector_child_mut!(self, 0, v);
    }
);

define_mutating_visitable!(
    ExprSetLiteral,
    self,
    v,
    {
        mutating_visit_vector_child!(self, 0, v);
    },
    {
        mutating_visit_vector_child_mut!(self, 0, v);
    }
);

// ExprRange
define_visitable!(
    ExprRange,
    self,
    v,
    {
        visit_child!(self, 0, v);
        visit_child!(self, 1, v);
    },
    {
        visit_child_mut!(self, 0, v);
        visit_child_mut!(self, 1, v);
    }
);

define_mutating_visitable!(
    ExprRange,
    self,
    v,
    {
        mutating_visit_child!(self, 0, v);
        mutating_visit_child!(self, 1, v);
    },
    {
        mutating_visit_child_mut!(self, 0, v);
        mutating_visit_child_mut!(self, 1, v);
    }
);

// ExprFunctionCall
define_visitable!(
    ExprFunctionCall,
    self,
    v,
    {
        visit_vector_child!(self, 1, v);
    },
    {
        visit_vector_child_mut!(self, 1, v);
    }
);

define_mutating_visitable!(
    ExprFunctionCall,
    self,
    v,
    {
        mutating_visit_vector_child!(self, 1, v);
    },
    {
        mutating_visit_vector_child_mut!(self, 1, v);
    }
);

// ExprVariable
define_visitable!(
    ExprVariable,
    self,
    v,
    {
        visit_child!(self, 0, v);
    },
    {
        visit_child_mut!(self, 0, v);
    }
);

define_mutating_visitable!(
    ExprVariable,
    self,
    v,
    {
        mutating_visit_child!(self, 0, v);
    },
    {
        mutating_visit_child_mut!(self, 0, v);
    }
);

// ExprVariableReference
define_visitable!(
    ExprVariableReference,
    self,
    v,
    {
        visit_child!(self, 0, v);
    },
    {
        visit_child_mut!(self, 0, v);
    }
);

define_mutating_visitable!(
    ExprVariableReference,
    self,
    v,
    {
        mutating_visit_child!(self, 0, v);
    },
    {
        mutating_visit_child_mut!(self, 0, v);
    }
);

// ExprParentheses
define_visitable!(
    ExprParentheses,
    self,
    v,
    {
        visit_child!(self, 0, v);
    },
    {
        visit_child_mut!(self, 0, v);
    }
);

define_mutating_visitable!(
    ExprParentheses,
    self,
    v,
    {
        mutating_visit_child!(self, 0, v);
    },
    {
        mutating_visit_child_mut!(self, 0, v);
    }
);

// ExprUnOp
define_visitable!(
    ExprUnOp,
    self,
    v,
    {
        visit_child!(self, 1, v);
    },
    {
        visit_child_mut!(self, 1, v);
    }
);

define_mutating_visitable!(
    ExprUnOp,
    self,
    v,
    {
        mutating_visit_child!(self, 1, v);
    },
    {
        mutating_visit_child_mut!(self, 1, v);
    }
);

// ExprBinOp
define_visitable!(
    ExprBinOp,
    self,
    v,
    {
        visit_child!(self, 1, v);
        visit_child!(self, 2, v);
    },
    {
        visit_child_mut!(self, 1, v);
        visit_child_mut!(self, 2, v);
    }
);

define_mutating_visitable!(
    ExprBinOp,
    self,
    v,
    {
        mutating_visit_child!(self, 1, v);
        mutating_visit_child!(self, 2, v);
    },
    {
        mutating_visit_child_mut!(self, 1, v);
        mutating_visit_child_mut!(self, 2, v);
    }
);

// ExprWriteParameter
define_visitable!(
    ExprWriteParameter,
    self,
    v,
    {
        visit_child!(self, 0, v);
        visit_child!(self, 1, v);
        visit_optional_child!(self, 2, v);
    },
    {
        visit_child_mut!(self, 0, v);
        visit_child_mut!(self, 1, v);
        visit_optional_child_mut!(self, 2, v);
    }
);

define_mutating_visitable!(
    ExprWriteParameter,
    self,
    v,
    {
        mutating_visit_child!(self, 0, v);
        mutating_visit_child!(self, 1, v);
        mutating_visit_optional_child!(self, 2, v);
    },
    {
        mutating_visit_child_mut!(self, 0, v);
        mutating_visit_child_mut!(self, 1, v);
        mutating_visit_optional_child_mut!(self, 2, v);
    }
);

// Conversion
define_visitable!(
    ExprConversion,
    self,
    v,
    {
        visit_child!(self, 0, v);
    },
    {
        visit_child_mut!(self, 0, v);
    }
);

define_mutating_visitable!(
    ExprConversion,
    self,
    v,
    {
        mutating_visit_child!(self, 0, v);
    },
    {
        mutating_visit_child_mut!(self, 0, v);
    }
);
