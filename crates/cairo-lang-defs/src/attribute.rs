use std::fmt;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_syntax::node::ast::{
    AttributeList, FunctionWithBody, ImplItem, Item, ItemConstant, ItemEnum, ItemExternFunction,
    ItemExternType, ItemImpl, ItemImplAlias, ItemModule, ItemStruct, ItemTrait, ItemTypeAlias,
    ItemUse, Member, TraitItemFunction,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use smol_str::SmolStr;

// TODO(yg): move to syntax (helpers? or a new file), impl for ast::Attribute and for
// defs::Attribute.
pub trait AttributeTrait<'db> {
    type DbType: ?Sized;

    fn name(&self, db: &'db Self::DbType) -> String;
    fn args(&self, db: &'db Self::DbType) -> String;
    fn full_text(&self, db: &'db Self::DbType) -> String {
        if self.args(db).is_empty() {
            self.name(db)
        } else {
            format!("{}({})", self.name(db), self.args(db))
        }
    }
    fn format(&self, db: &'db Self::DbType) -> String {
        format!("#[{}]", self.full_text(db))
    }
}
impl<'db> AttributeTrait<'db> for Attribute {
    type DbType = dyn SyntaxGroup;
    fn name(&self, _db: &'db Self::DbType) -> String {
        self.id.to_string()
    }
    fn args(&self, db: &'db Self::DbType) -> String {
        self.args.iter().map(|arg| arg.text(db)).collect::<Vec<_>>().join(", ")
    }
}
impl<'db> AttributeTrait<'db> for ast::Attribute {
    type DbType = dyn SyntaxGroup;
    fn name(&self, db: &'db Self::DbType) -> String {
        self.attr(db).as_syntax_node().get_text_without_trivia(db)
    }
    fn args(&self, db: &'db Self::DbType) -> String {
        self.arguments(db).as_syntax_node().get_text_without_trivia(db)
    }
}

/// Easier to digest representation of an [ast::Attribute].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Attribute {
    pub stable_ptr: ast::AttributePtr,
    pub id: SmolStr,
    pub id_stable_ptr: ast::ExprPathPtr,
    pub args: Vec<AttributeArg>,
    pub args_stable_ptr: ast::OptionArgListParenthesizedPtr,
}

// TODO(yg): needed?
// pub struct AttributeList(Vec<Attribute>);
// impl QueryAttrs for AttributeList {
//     type AttrType = Attribute;
//     fn attributes_elements(&self, db: &dyn SyntaxGroup) -> Vec<Self::AttrType> {
//         self.elements(db)
//     }
// }

/// Easier to digest representation of a single attribute value.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AttributeArg {
    pub variant: AttributeArgVariant,
    pub arg: ast::Arg,
    pub arg_stable_ptr: ast::ArgPtr,
    pub modifiers: Vec<Modifier>,
}

/// Variant of [`AttributeArg`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AttributeArgVariant {
    /// Just `value`.
    Unnamed { value: ast::Expr, value_stable_ptr: ast::ExprPtr },
    /// `name: value`.
    Named {
        value: ast::Expr,
        value_stable_ptr: ast::ExprPtr,
        name: SmolStr,
        name_stable_ptr: ast::TerminalIdentifierPtr,
    },
    /// `:name`
    FieldInitShorthand { name: SmolStr, name_stable_ptr: ast::TerminalIdentifierPtr },
}

/// Easier to digest representation of a [`ast::Modifier`] attached to [`AttributeArg`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Modifier {
    pub text: SmolStr,
    pub stable_ptr: ast::ModifierPtr,
}

impl<'a> DebugWithDb<dyn SyntaxGroup + 'a> for Attribute {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &(dyn SyntaxGroup + 'a)) -> fmt::Result {
        write!(f, r#"Attribute {{ id: "{}""#, self.id)?;
        if !self.args.is_empty() {
            write!(f, ", args: [")?;
            for arg in self.args.iter() {
                write!(f, "{:?}, ", arg.arg.as_syntax_node().get_text(db))?;
            }
            write!(f, "]")?;
        }
        write!(f, " }}")
    }
}

pub trait AttributeStructurize {
    /// Return the structured attribute for the given [ast::Attribute].
    fn structurize(self, db: &dyn SyntaxGroup) -> Attribute;
}

impl AttributeStructurize for ast::Attribute {
    fn structurize(self, db: &dyn SyntaxGroup) -> Attribute {
        let attr_id = self.attr(db);
        let attr_args = self.arguments(db);

        Attribute {
            stable_ptr: self.stable_ptr(),
            id: attr_id.as_syntax_node().get_text_without_trivia(db).into(),
            id_stable_ptr: attr_id.stable_ptr(),
            args: match attr_args {
                ast::OptionArgListParenthesized::ArgListParenthesized(ref attribute_args) => {
                    attribute_args
                        .args(db)
                        .elements(db)
                        .into_iter()
                        .map(|arg| AttributeArg::from(arg, db))
                        .collect()
                }
                ast::OptionArgListParenthesized::Empty(_) => vec![],
            },
            args_stable_ptr: attr_args.stable_ptr(),
        }
    }
}

pub trait AttributeListStructurize {
    /// Return structured attributes for the given [ast::AttributeList].
    fn structurize(self, db: &dyn SyntaxGroup) -> Vec<Attribute>;
}

impl AttributeListStructurize for ast::AttributeList {
    fn structurize(self, db: &dyn SyntaxGroup) -> Vec<Attribute> {
        // TODO(ilya): Consider checking for attribute repetitions.
        self.elements(db).into_iter().map(|attr| attr.structurize(db)).collect()
    }
}

impl AttributeArg {
    /// Build [`AttributeArg`] from [`ast::Arg`].
    fn from(arg: ast::Arg, db: &dyn SyntaxGroup) -> AttributeArg {
        let variant = match arg.arg_clause(db) {
            ast::ArgClause::Unnamed(clause) => {
                let value = clause.value(db);
                AttributeArgVariant::Unnamed { value_stable_ptr: value.stable_ptr(), value }
            }
            ast::ArgClause::Named(clause) => {
                let identifier = clause.name(db);
                let value = clause.value(db);
                AttributeArgVariant::Named {
                    value_stable_ptr: value.stable_ptr(),
                    value,
                    name_stable_ptr: identifier.stable_ptr(),
                    name: identifier.text(db),
                }
            }
            ast::ArgClause::FieldInitShorthand(clause) => {
                let identifier = clause.name(db).name(db);
                AttributeArgVariant::FieldInitShorthand {
                    name_stable_ptr: identifier.stable_ptr(),
                    name: identifier.text(db),
                }
            }
        };

        let modifiers = arg
            .modifiers(db)
            .elements(db)
            .into_iter()
            .map(|modifier| Modifier::from(modifier, db))
            .collect();

        let arg_stable_ptr = arg.stable_ptr();
        AttributeArg { variant, arg, arg_stable_ptr, modifiers }
    }

    fn text(&self, db: &dyn SyntaxGroup) -> String {
        match &self.variant {
            AttributeArgVariant::Unnamed { value, .. } => {
                value.as_syntax_node().get_text_without_trivia(db)
            }
            AttributeArgVariant::Named { value, name, .. } => {
                format!("{}: {}", name, value.as_syntax_node().get_text_without_trivia(db))
            }
            AttributeArgVariant::FieldInitShorthand { name, .. } => {
                format!(":{}", name)
            }
        }
    }
}

impl Modifier {
    /// Build [`Modifier`] from [`ast::Modifier`].
    fn from(modifier: ast::Modifier, db: &dyn SyntaxGroup) -> Modifier {
        Modifier {
            stable_ptr: modifier.stable_ptr(),
            text: modifier.as_syntax_node().get_text(db).into(),
        }
    }
}

/// Trait for querying attributes of AST items.
pub trait QueryAttrs<'db> {
    type AttrType: AttributeTrait<'db, DbType = Self::DbType>;
    type DbType: ?Sized + 'db;

    /// Generic call `self.attributes(db).elements(db)`.
    ///
    /// Implementation detail, should not be used by this trait users.
    #[doc(hidden)]
    fn attributes_elements(&self, db: &'db Self::DbType) -> Vec<Self::AttrType>;

    /// Collect all attributes attached to this node whose name (without args) is exactly `attr`.
    fn query_attr(&self, db: &'db Self::DbType, attr: &str) -> Vec<Self::AttrType> {
        self.attributes_elements(db).into_iter().filter(|a| a.name(db) == attr).collect()
    }

    /// Find first attribute attached to this node whose name (without args) is exactly `attr`.
    fn find_attr(&self, db: &'db Self::DbType, attr: &str) -> Option<Self::AttrType> {
        self.query_attr(db, attr).into_iter().next()
    }

    /// Check if this node has an attribute whose name (without args) is exactly `attr`.
    fn has_attr(&self, db: &'db Self::DbType, attr: &str) -> bool {
        self.find_attr(db, attr).is_some()
    }
}
impl<'db> QueryAttrs<'db> for ItemConstant {
    type AttrType = ast::Attribute;
    type DbType = dyn SyntaxGroup;
    fn attributes_elements(&self, db: &'db Self::DbType) -> Vec<Self::AttrType> {
        self.attributes(db).elements(db)
    }
}
impl<'db> QueryAttrs<'db> for ItemModule {
    type AttrType = ast::Attribute;
    type DbType = dyn SyntaxGroup;
    fn attributes_elements(&self, db: &'db Self::DbType) -> Vec<Self::AttrType> {
        self.attributes(db).elements(db)
    }
}
impl<'db> QueryAttrs<'db> for FunctionWithBody {
    type AttrType = ast::Attribute;
    type DbType = dyn SyntaxGroup;
    fn attributes_elements(&self, db: &'db Self::DbType) -> Vec<Self::AttrType> {
        self.attributes(db).elements(db)
    }
}
impl<'db> QueryAttrs<'db> for ItemUse {
    type AttrType = ast::Attribute;
    type DbType = dyn SyntaxGroup;
    fn attributes_elements(&self, db: &'db Self::DbType) -> Vec<Self::AttrType> {
        self.attributes(db).elements(db)
    }
}
impl<'db> QueryAttrs<'db> for ItemExternFunction {
    type AttrType = ast::Attribute;
    type DbType = dyn SyntaxGroup;
    fn attributes_elements(&self, db: &'db Self::DbType) -> Vec<Self::AttrType> {
        self.attributes(db).elements(db)
    }
}
impl<'db> QueryAttrs<'db> for ItemExternType {
    type AttrType = ast::Attribute;
    type DbType = dyn SyntaxGroup;
    fn attributes_elements(&self, db: &'db Self::DbType) -> Vec<Self::AttrType> {
        self.attributes(db).elements(db)
    }
}
impl<'db> QueryAttrs<'db> for ItemTrait {
    type AttrType = ast::Attribute;
    type DbType = dyn SyntaxGroup;
    fn attributes_elements(&self, db: &'db Self::DbType) -> Vec<Self::AttrType> {
        self.attributes(db).elements(db)
    }
}
impl<'db> QueryAttrs<'db> for ItemImpl {
    type AttrType = ast::Attribute;
    type DbType = dyn SyntaxGroup;
    fn attributes_elements(&self, db: &'db Self::DbType) -> Vec<Self::AttrType> {
        self.attributes(db).elements(db)
    }
}
impl<'db> QueryAttrs<'db> for ItemImplAlias {
    type AttrType = ast::Attribute;
    type DbType = dyn SyntaxGroup;
    fn attributes_elements(&self, db: &'db Self::DbType) -> Vec<Self::AttrType> {
        self.attributes(db).elements(db)
    }
}
impl<'db> QueryAttrs<'db> for ItemStruct {
    type AttrType = ast::Attribute;
    type DbType = dyn SyntaxGroup;
    fn attributes_elements(&self, db: &'db Self::DbType) -> Vec<Self::AttrType> {
        self.attributes(db).elements(db)
    }
}
impl<'db> QueryAttrs<'db> for ItemEnum {
    type AttrType = ast::Attribute;
    type DbType = dyn SyntaxGroup;
    fn attributes_elements(&self, db: &'db Self::DbType) -> Vec<Self::AttrType> {
        self.attributes(db).elements(db)
    }
}
impl<'db> QueryAttrs<'db> for ItemTypeAlias {
    type AttrType = ast::Attribute;
    type DbType = dyn SyntaxGroup;
    fn attributes_elements(&self, db: &'db Self::DbType) -> Vec<Self::AttrType> {
        self.attributes(db).elements(db)
    }
}
impl<'db> QueryAttrs<'db> for TraitItemFunction {
    type AttrType = ast::Attribute;
    type DbType = dyn SyntaxGroup;
    fn attributes_elements(&self, db: &'db Self::DbType) -> Vec<Self::AttrType> {
        self.attributes(db).elements(db)
    }
}

impl<'db> QueryAttrs<'db> for Item {
    type AttrType = ast::Attribute;
    type DbType = dyn SyntaxGroup;
    fn attributes_elements(&self, db: &'db Self::DbType) -> Vec<Self::AttrType> {
        match self {
            Item::Constant(item) => item.attributes_elements(db),
            Item::Module(item) => item.attributes_elements(db),
            Item::FreeFunction(item) => item.attributes_elements(db),
            Item::Use(item) => item.attributes_elements(db),
            Item::ExternFunction(item) => item.attributes_elements(db),
            Item::ExternType(item) => item.attributes_elements(db),
            Item::Trait(item) => item.attributes_elements(db),
            Item::Impl(item) => item.attributes_elements(db),
            Item::ImplAlias(item) => item.attributes_elements(db),
            Item::Struct(item) => item.attributes_elements(db),
            Item::Enum(item) => item.attributes_elements(db),
            Item::TypeAlias(item) => item.attributes_elements(db),
            Item::Missing(_) => vec![],
        }
    }
}

impl<'db> QueryAttrs<'db> for ImplItem {
    type AttrType = ast::Attribute;
    type DbType = dyn SyntaxGroup;
    fn attributes_elements(&self, db: &'db Self::DbType) -> Vec<Self::AttrType> {
        match self {
            ImplItem::Function(item) => item.attributes_elements(db),
            ImplItem::Constant(item) => item.attributes_elements(db),
            ImplItem::Module(item) => item.attributes_elements(db),
            ImplItem::Use(item) => item.attributes_elements(db),
            ImplItem::ExternFunction(item) => item.attributes_elements(db),
            ImplItem::ExternType(item) => item.attributes_elements(db),
            ImplItem::Trait(item) => item.attributes_elements(db),
            ImplItem::Impl(item) => item.attributes_elements(db),
            ImplItem::ImplAlias(item) => item.attributes_elements(db),
            ImplItem::Struct(item) => item.attributes_elements(db),
            ImplItem::Enum(item) => item.attributes_elements(db),
            ImplItem::TypeAlias(item) => item.attributes_elements(db),
            ImplItem::Missing(_) => vec![],
        }
    }
}

impl<'db> QueryAttrs<'db> for AttributeList {
    type AttrType = ast::Attribute;
    type DbType = dyn SyntaxGroup;
    fn attributes_elements(&self, db: &'db Self::DbType) -> Vec<Self::AttrType> {
        self.elements(db)
    }
}

impl<'db> QueryAttrs<'db> for Member {
    type AttrType = ast::Attribute;
    type DbType = dyn SyntaxGroup;
    fn attributes_elements(&self, db: &'db Self::DbType) -> Vec<Self::AttrType> {
        self.attributes(db).elements(db)
    }
}
