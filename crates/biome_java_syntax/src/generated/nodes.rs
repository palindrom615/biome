//! Generated file, do not edit by hand, see `xtask/codegen`

#![allow(clippy::enum_variant_names)]
#![allow(clippy::match_like_matches_macro)]
use crate::{
    macros::map_syntax_node,
    JavaLanguage as Language, JavaSyntaxElement as SyntaxElement,
    JavaSyntaxElementChildren as SyntaxElementChildren,
    JavaSyntaxKind::{self as SyntaxKind, *},
    JavaSyntaxList as SyntaxList, JavaSyntaxNode as SyntaxNode, JavaSyntaxToken as SyntaxToken,
};
use biome_rowan::{support, AstNode, RawSyntaxKind, SyntaxKindSet, SyntaxResult};
#[allow(unused)]
use biome_rowan::{
    AstNodeList, AstNodeListIterator, AstNodeSlotMap, AstSeparatedList,
    AstSeparatedListNodesIterator,
};
use serde::ser::SerializeSeq;
use serde::{Serialize, Serializer};
use std::fmt::{Debug, Formatter};
#[doc = r" Sentinel value indicating a missing element in a dynamic node, where"]
#[doc = r" the slots are not statically known."]
#[allow(dead_code)]
pub(crate) const SLOT_MAP_EMPTY_VALUE: u8 = u8::MAX;
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct JavaClass {
    pub(crate) syntax: SyntaxNode,
}
impl JavaClass {
    #[doc = r" Create an AstNode from a SyntaxNode without checking its kind"]
    #[doc = r""]
    #[doc = r" # Safety"]
    #[doc = r" This function must be guarded with a call to [AstNode::can_cast]"]
    #[doc = r" or a match on [SyntaxNode::kind]"]
    #[inline]
    pub const unsafe fn new_unchecked(syntax: SyntaxNode) -> Self {
        Self { syntax }
    }
    pub fn as_fields(&self) -> JavaClassFields {
        JavaClassFields {
            class_token: self.class_token(),
            name_token: self.name_token(),
            body: self.body(),
        }
    }
    pub fn class_token(&self) -> SyntaxResult<SyntaxToken> {
        support::required_token(&self.syntax, 0usize)
    }
    pub fn name_token(&self) -> SyntaxResult<SyntaxToken> {
        support::required_token(&self.syntax, 1usize)
    }
    pub fn body(&self) -> SyntaxResult<JavaClassBody> {
        support::required_node(&self.syntax, 2usize)
    }
}
impl Serialize for JavaClass {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.as_fields().serialize(serializer)
    }
}
#[derive(Serialize)]
pub struct JavaClassFields {
    pub class_token: SyntaxResult<SyntaxToken>,
    pub name_token: SyntaxResult<SyntaxToken>,
    pub body: SyntaxResult<JavaClassBody>,
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct JavaClassBody {
    pub(crate) syntax: SyntaxNode,
}
impl JavaClassBody {
    #[doc = r" Create an AstNode from a SyntaxNode without checking its kind"]
    #[doc = r""]
    #[doc = r" # Safety"]
    #[doc = r" This function must be guarded with a call to [AstNode::can_cast]"]
    #[doc = r" or a match on [SyntaxNode::kind]"]
    #[inline]
    pub const unsafe fn new_unchecked(syntax: SyntaxNode) -> Self {
        Self { syntax }
    }
    pub fn as_fields(&self) -> JavaClassBodyFields {
        JavaClassBodyFields {
            l_curly_token: self.l_curly_token(),
            members: self.members(),
            r_curly_token: self.r_curly_token(),
        }
    }
    pub fn l_curly_token(&self) -> SyntaxResult<SyntaxToken> {
        support::required_token(&self.syntax, 0usize)
    }
    pub fn members(&self) -> JavaClassMemberList {
        support::list(&self.syntax, 1usize)
    }
    pub fn r_curly_token(&self) -> SyntaxResult<SyntaxToken> {
        support::required_token(&self.syntax, 2usize)
    }
}
impl Serialize for JavaClassBody {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.as_fields().serialize(serializer)
    }
}
#[derive(Serialize)]
pub struct JavaClassBodyFields {
    pub l_curly_token: SyntaxResult<SyntaxToken>,
    pub members: JavaClassMemberList,
    pub r_curly_token: SyntaxResult<SyntaxToken>,
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct JavaExpressionStatement {
    pub(crate) syntax: SyntaxNode,
}
impl JavaExpressionStatement {
    #[doc = r" Create an AstNode from a SyntaxNode without checking its kind"]
    #[doc = r""]
    #[doc = r" # Safety"]
    #[doc = r" This function must be guarded with a call to [AstNode::can_cast]"]
    #[doc = r" or a match on [SyntaxNode::kind]"]
    #[inline]
    pub const unsafe fn new_unchecked(syntax: SyntaxNode) -> Self {
        Self { syntax }
    }
    pub fn as_fields(&self) -> JavaExpressionStatementFields {
        JavaExpressionStatementFields {
            expression_token: self.expression_token(),
            semicolon_token: self.semicolon_token(),
        }
    }
    pub fn expression_token(&self) -> SyntaxResult<SyntaxToken> {
        support::required_token(&self.syntax, 0usize)
    }
    pub fn semicolon_token(&self) -> SyntaxResult<SyntaxToken> {
        support::required_token(&self.syntax, 1usize)
    }
}
impl Serialize for JavaExpressionStatement {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.as_fields().serialize(serializer)
    }
}
#[derive(Serialize)]
pub struct JavaExpressionStatementFields {
    pub expression_token: SyntaxResult<SyntaxToken>,
    pub semicolon_token: SyntaxResult<SyntaxToken>,
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct JavaField {
    pub(crate) syntax: SyntaxNode,
}
impl JavaField {
    #[doc = r" Create an AstNode from a SyntaxNode without checking its kind"]
    #[doc = r""]
    #[doc = r" # Safety"]
    #[doc = r" This function must be guarded with a call to [AstNode::can_cast]"]
    #[doc = r" or a match on [SyntaxNode::kind]"]
    #[inline]
    pub const unsafe fn new_unchecked(syntax: SyntaxNode) -> Self {
        Self { syntax }
    }
    pub fn as_fields(&self) -> JavaFieldFields {
        JavaFieldFields {
            field_token: self.field_token(),
            name_token: self.name_token(),
            type_token: self.type_token(),
            semicolon_token: self.semicolon_token(),
        }
    }
    pub fn field_token(&self) -> SyntaxResult<SyntaxToken> {
        support::required_token(&self.syntax, 0usize)
    }
    pub fn name_token(&self) -> SyntaxResult<SyntaxToken> {
        support::required_token(&self.syntax, 1usize)
    }
    pub fn type_token(&self) -> SyntaxResult<SyntaxToken> {
        support::required_token(&self.syntax, 2usize)
    }
    pub fn semicolon_token(&self) -> SyntaxResult<SyntaxToken> {
        support::required_token(&self.syntax, 3usize)
    }
}
impl Serialize for JavaField {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.as_fields().serialize(serializer)
    }
}
#[derive(Serialize)]
pub struct JavaFieldFields {
    pub field_token: SyntaxResult<SyntaxToken>,
    pub name_token: SyntaxResult<SyntaxToken>,
    pub type_token: SyntaxResult<SyntaxToken>,
    pub semicolon_token: SyntaxResult<SyntaxToken>,
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct JavaImport {
    pub(crate) syntax: SyntaxNode,
}
impl JavaImport {
    #[doc = r" Create an AstNode from a SyntaxNode without checking its kind"]
    #[doc = r""]
    #[doc = r" # Safety"]
    #[doc = r" This function must be guarded with a call to [AstNode::can_cast]"]
    #[doc = r" or a match on [SyntaxNode::kind]"]
    #[inline]
    pub const unsafe fn new_unchecked(syntax: SyntaxNode) -> Self {
        Self { syntax }
    }
    pub fn as_fields(&self) -> JavaImportFields {
        JavaImportFields {
            import_token: self.import_token(),
            name_token: self.name_token(),
            semicolon_token: self.semicolon_token(),
        }
    }
    pub fn import_token(&self) -> SyntaxResult<SyntaxToken> {
        support::required_token(&self.syntax, 0usize)
    }
    pub fn name_token(&self) -> SyntaxResult<SyntaxToken> {
        support::required_token(&self.syntax, 1usize)
    }
    pub fn semicolon_token(&self) -> SyntaxResult<SyntaxToken> {
        support::required_token(&self.syntax, 2usize)
    }
}
impl Serialize for JavaImport {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.as_fields().serialize(serializer)
    }
}
#[derive(Serialize)]
pub struct JavaImportFields {
    pub import_token: SyntaxResult<SyntaxToken>,
    pub name_token: SyntaxResult<SyntaxToken>,
    pub semicolon_token: SyntaxResult<SyntaxToken>,
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct JavaLiteral {
    pub(crate) syntax: SyntaxNode,
}
impl JavaLiteral {
    #[doc = r" Create an AstNode from a SyntaxNode without checking its kind"]
    #[doc = r""]
    #[doc = r" # Safety"]
    #[doc = r" This function must be guarded with a call to [AstNode::can_cast]"]
    #[doc = r" or a match on [SyntaxNode::kind]"]
    #[inline]
    pub const unsafe fn new_unchecked(syntax: SyntaxNode) -> Self {
        Self { syntax }
    }
    pub fn as_fields(&self) -> JavaLiteralFields {
        JavaLiteralFields {
            literal_token: self.literal_token(),
            value_token: self.value_token(),
            semicolon_token: self.semicolon_token(),
        }
    }
    pub fn literal_token(&self) -> SyntaxResult<SyntaxToken> {
        support::required_token(&self.syntax, 0usize)
    }
    pub fn value_token(&self) -> SyntaxResult<SyntaxToken> {
        support::required_token(&self.syntax, 1usize)
    }
    pub fn semicolon_token(&self) -> SyntaxResult<SyntaxToken> {
        support::required_token(&self.syntax, 2usize)
    }
}
impl Serialize for JavaLiteral {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.as_fields().serialize(serializer)
    }
}
#[derive(Serialize)]
pub struct JavaLiteralFields {
    pub literal_token: SyntaxResult<SyntaxToken>,
    pub value_token: SyntaxResult<SyntaxToken>,
    pub semicolon_token: SyntaxResult<SyntaxToken>,
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct JavaMethod {
    pub(crate) syntax: SyntaxNode,
}
impl JavaMethod {
    #[doc = r" Create an AstNode from a SyntaxNode without checking its kind"]
    #[doc = r""]
    #[doc = r" # Safety"]
    #[doc = r" This function must be guarded with a call to [AstNode::can_cast]"]
    #[doc = r" or a match on [SyntaxNode::kind]"]
    #[inline]
    pub const unsafe fn new_unchecked(syntax: SyntaxNode) -> Self {
        Self { syntax }
    }
    pub fn as_fields(&self) -> JavaMethodFields {
        JavaMethodFields {
            method_token: self.method_token(),
            name_token: self.name_token(),
            returnType_token: self.returnType_token(),
            body: self.body(),
        }
    }
    pub fn method_token(&self) -> SyntaxResult<SyntaxToken> {
        support::required_token(&self.syntax, 0usize)
    }
    pub fn name_token(&self) -> SyntaxResult<SyntaxToken> {
        support::required_token(&self.syntax, 1usize)
    }
    pub fn returnType_token(&self) -> SyntaxResult<SyntaxToken> {
        support::required_token(&self.syntax, 2usize)
    }
    pub fn body(&self) -> SyntaxResult<JavaMethodBody> {
        support::required_node(&self.syntax, 3usize)
    }
}
impl Serialize for JavaMethod {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.as_fields().serialize(serializer)
    }
}
#[derive(Serialize)]
pub struct JavaMethodFields {
    pub method_token: SyntaxResult<SyntaxToken>,
    pub name_token: SyntaxResult<SyntaxToken>,
    pub returnType_token: SyntaxResult<SyntaxToken>,
    pub body: SyntaxResult<JavaMethodBody>,
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct JavaMethodBody {
    pub(crate) syntax: SyntaxNode,
}
impl JavaMethodBody {
    #[doc = r" Create an AstNode from a SyntaxNode without checking its kind"]
    #[doc = r""]
    #[doc = r" # Safety"]
    #[doc = r" This function must be guarded with a call to [AstNode::can_cast]"]
    #[doc = r" or a match on [SyntaxNode::kind]"]
    #[inline]
    pub const unsafe fn new_unchecked(syntax: SyntaxNode) -> Self {
        Self { syntax }
    }
    pub fn as_fields(&self) -> JavaMethodBodyFields {
        JavaMethodBodyFields {
            l_curly_token: self.l_curly_token(),
            statements: self.statements(),
            r_curly_token: self.r_curly_token(),
        }
    }
    pub fn l_curly_token(&self) -> SyntaxResult<SyntaxToken> {
        support::required_token(&self.syntax, 0usize)
    }
    pub fn statements(&self) -> JavaStatementList {
        support::list(&self.syntax, 1usize)
    }
    pub fn r_curly_token(&self) -> SyntaxResult<SyntaxToken> {
        support::required_token(&self.syntax, 2usize)
    }
}
impl Serialize for JavaMethodBody {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.as_fields().serialize(serializer)
    }
}
#[derive(Serialize)]
pub struct JavaMethodBodyFields {
    pub l_curly_token: SyntaxResult<SyntaxToken>,
    pub statements: JavaStatementList,
    pub r_curly_token: SyntaxResult<SyntaxToken>,
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct JavaMethodCall {
    pub(crate) syntax: SyntaxNode,
}
impl JavaMethodCall {
    #[doc = r" Create an AstNode from a SyntaxNode without checking its kind"]
    #[doc = r""]
    #[doc = r" # Safety"]
    #[doc = r" This function must be guarded with a call to [AstNode::can_cast]"]
    #[doc = r" or a match on [SyntaxNode::kind]"]
    #[inline]
    pub const unsafe fn new_unchecked(syntax: SyntaxNode) -> Self {
        Self { syntax }
    }
    pub fn as_fields(&self) -> JavaMethodCallFields {
        JavaMethodCallFields {
            methodCall_token: self.methodCall_token(),
            name_token: self.name_token(),
            l_paren_token: self.l_paren_token(),
            arguments: self.arguments(),
            r_paren_token: self.r_paren_token(),
            semicolon_token: self.semicolon_token(),
        }
    }
    pub fn methodCall_token(&self) -> SyntaxResult<SyntaxToken> {
        support::required_token(&self.syntax, 0usize)
    }
    pub fn name_token(&self) -> SyntaxResult<SyntaxToken> {
        support::required_token(&self.syntax, 1usize)
    }
    pub fn l_paren_token(&self) -> SyntaxResult<SyntaxToken> {
        support::required_token(&self.syntax, 2usize)
    }
    pub fn arguments(&self) -> JavaExpressionList {
        support::list(&self.syntax, 3usize)
    }
    pub fn r_paren_token(&self) -> SyntaxResult<SyntaxToken> {
        support::required_token(&self.syntax, 4usize)
    }
    pub fn semicolon_token(&self) -> SyntaxResult<SyntaxToken> {
        support::required_token(&self.syntax, 5usize)
    }
}
impl Serialize for JavaMethodCall {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.as_fields().serialize(serializer)
    }
}
#[derive(Serialize)]
pub struct JavaMethodCallFields {
    pub methodCall_token: SyntaxResult<SyntaxToken>,
    pub name_token: SyntaxResult<SyntaxToken>,
    pub l_paren_token: SyntaxResult<SyntaxToken>,
    pub arguments: JavaExpressionList,
    pub r_paren_token: SyntaxResult<SyntaxToken>,
    pub semicolon_token: SyntaxResult<SyntaxToken>,
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct JavaPackage {
    pub(crate) syntax: SyntaxNode,
}
impl JavaPackage {
    #[doc = r" Create an AstNode from a SyntaxNode without checking its kind"]
    #[doc = r""]
    #[doc = r" # Safety"]
    #[doc = r" This function must be guarded with a call to [AstNode::can_cast]"]
    #[doc = r" or a match on [SyntaxNode::kind]"]
    #[inline]
    pub const unsafe fn new_unchecked(syntax: SyntaxNode) -> Self {
        Self { syntax }
    }
    pub fn as_fields(&self) -> JavaPackageFields {
        JavaPackageFields {
            package_token: self.package_token(),
            name_token: self.name_token(),
            semicolon_token: self.semicolon_token(),
        }
    }
    pub fn package_token(&self) -> SyntaxResult<SyntaxToken> {
        support::required_token(&self.syntax, 0usize)
    }
    pub fn name_token(&self) -> SyntaxResult<SyntaxToken> {
        support::required_token(&self.syntax, 1usize)
    }
    pub fn semicolon_token(&self) -> SyntaxResult<SyntaxToken> {
        support::required_token(&self.syntax, 2usize)
    }
}
impl Serialize for JavaPackage {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.as_fields().serialize(serializer)
    }
}
#[derive(Serialize)]
pub struct JavaPackageFields {
    pub package_token: SyntaxResult<SyntaxToken>,
    pub name_token: SyntaxResult<SyntaxToken>,
    pub semicolon_token: SyntaxResult<SyntaxToken>,
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct JavaReturnStatement {
    pub(crate) syntax: SyntaxNode,
}
impl JavaReturnStatement {
    #[doc = r" Create an AstNode from a SyntaxNode without checking its kind"]
    #[doc = r""]
    #[doc = r" # Safety"]
    #[doc = r" This function must be guarded with a call to [AstNode::can_cast]"]
    #[doc = r" or a match on [SyntaxNode::kind]"]
    #[inline]
    pub const unsafe fn new_unchecked(syntax: SyntaxNode) -> Self {
        Self { syntax }
    }
    pub fn as_fields(&self) -> JavaReturnStatementFields {
        JavaReturnStatementFields {
            return_token: self.return_token(),
            expression: self.expression(),
            semicolon_token: self.semicolon_token(),
        }
    }
    pub fn return_token(&self) -> SyntaxResult<SyntaxToken> {
        support::required_token(&self.syntax, 0usize)
    }
    pub fn expression(&self) -> SyntaxResult<JavaExpression> {
        support::required_node(&self.syntax, 1usize)
    }
    pub fn semicolon_token(&self) -> SyntaxResult<SyntaxToken> {
        support::required_token(&self.syntax, 2usize)
    }
}
impl Serialize for JavaReturnStatement {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.as_fields().serialize(serializer)
    }
}
#[derive(Serialize)]
pub struct JavaReturnStatementFields {
    pub return_token: SyntaxResult<SyntaxToken>,
    pub expression: SyntaxResult<JavaExpression>,
    pub semicolon_token: SyntaxResult<SyntaxToken>,
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct JavaRoot {
    pub(crate) syntax: SyntaxNode,
}
impl JavaRoot {
    #[doc = r" Create an AstNode from a SyntaxNode without checking its kind"]
    #[doc = r""]
    #[doc = r" # Safety"]
    #[doc = r" This function must be guarded with a call to [AstNode::can_cast]"]
    #[doc = r" or a match on [SyntaxNode::kind]"]
    #[inline]
    pub const unsafe fn new_unchecked(syntax: SyntaxNode) -> Self {
        Self { syntax }
    }
    pub fn as_fields(&self) -> JavaRootFields {
        JavaRootFields {
            bom_token: self.bom_token(),
            package: self.package(),
            imports: self.imports(),
            classes: self.classes(),
            eof_token: self.eof_token(),
        }
    }
    pub fn bom_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, 0usize)
    }
    pub fn package(&self) -> SyntaxResult<JavaPackage> {
        support::required_node(&self.syntax, 1usize)
    }
    pub fn imports(&self) -> JavaImportList {
        support::list(&self.syntax, 2usize)
    }
    pub fn classes(&self) -> JavaClassList {
        support::list(&self.syntax, 3usize)
    }
    pub fn eof_token(&self) -> SyntaxResult<SyntaxToken> {
        support::required_token(&self.syntax, 4usize)
    }
}
impl Serialize for JavaRoot {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.as_fields().serialize(serializer)
    }
}
#[derive(Serialize)]
pub struct JavaRootFields {
    pub bom_token: Option<SyntaxToken>,
    pub package: SyntaxResult<JavaPackage>,
    pub imports: JavaImportList,
    pub classes: JavaClassList,
    pub eof_token: SyntaxResult<SyntaxToken>,
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct JavaVariable {
    pub(crate) syntax: SyntaxNode,
}
impl JavaVariable {
    #[doc = r" Create an AstNode from a SyntaxNode without checking its kind"]
    #[doc = r""]
    #[doc = r" # Safety"]
    #[doc = r" This function must be guarded with a call to [AstNode::can_cast]"]
    #[doc = r" or a match on [SyntaxNode::kind]"]
    #[inline]
    pub const unsafe fn new_unchecked(syntax: SyntaxNode) -> Self {
        Self { syntax }
    }
    pub fn as_fields(&self) -> JavaVariableFields {
        JavaVariableFields {
            variable_token: self.variable_token(),
            name_token: self.name_token(),
            semicolon_token: self.semicolon_token(),
        }
    }
    pub fn variable_token(&self) -> SyntaxResult<SyntaxToken> {
        support::required_token(&self.syntax, 0usize)
    }
    pub fn name_token(&self) -> SyntaxResult<SyntaxToken> {
        support::required_token(&self.syntax, 1usize)
    }
    pub fn semicolon_token(&self) -> SyntaxResult<SyntaxToken> {
        support::required_token(&self.syntax, 2usize)
    }
}
impl Serialize for JavaVariable {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.as_fields().serialize(serializer)
    }
}
#[derive(Serialize)]
pub struct JavaVariableFields {
    pub variable_token: SyntaxResult<SyntaxToken>,
    pub name_token: SyntaxResult<SyntaxToken>,
    pub semicolon_token: SyntaxResult<SyntaxToken>,
}
#[derive(Clone, PartialEq, Eq, Hash, Serialize)]
pub enum JavaClassMember {
    JavaField(JavaField),
    JavaMethod(JavaMethod),
}
impl JavaClassMember {
    pub fn as_java_field(&self) -> Option<&JavaField> {
        match &self {
            JavaClassMember::JavaField(item) => Some(item),
            _ => None,
        }
    }
    pub fn as_java_method(&self) -> Option<&JavaMethod> {
        match &self {
            JavaClassMember::JavaMethod(item) => Some(item),
            _ => None,
        }
    }
}
#[derive(Clone, PartialEq, Eq, Hash, Serialize)]
pub enum JavaExpression {
    JavaLiteral(JavaLiteral),
    JavaMethodCall(JavaMethodCall),
    JavaVariable(JavaVariable),
}
impl JavaExpression {
    pub fn as_java_literal(&self) -> Option<&JavaLiteral> {
        match &self {
            JavaExpression::JavaLiteral(item) => Some(item),
            _ => None,
        }
    }
    pub fn as_java_method_call(&self) -> Option<&JavaMethodCall> {
        match &self {
            JavaExpression::JavaMethodCall(item) => Some(item),
            _ => None,
        }
    }
    pub fn as_java_variable(&self) -> Option<&JavaVariable> {
        match &self {
            JavaExpression::JavaVariable(item) => Some(item),
            _ => None,
        }
    }
}
#[derive(Clone, PartialEq, Eq, Hash, Serialize)]
pub enum JavaStatement {
    JavaExpressionStatement(JavaExpressionStatement),
    JavaReturnStatement(JavaReturnStatement),
}
impl JavaStatement {
    pub fn as_java_expression_statement(&self) -> Option<&JavaExpressionStatement> {
        match &self {
            JavaStatement::JavaExpressionStatement(item) => Some(item),
            _ => None,
        }
    }
    pub fn as_java_return_statement(&self) -> Option<&JavaReturnStatement> {
        match &self {
            JavaStatement::JavaReturnStatement(item) => Some(item),
            _ => None,
        }
    }
}
impl AstNode for JavaClass {
    type Language = Language;
    const KIND_SET: SyntaxKindSet<Language> =
        SyntaxKindSet::from_raw(RawSyntaxKind(JAVA_CLASS as u16));
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == JAVA_CLASS
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
    fn into_syntax(self) -> SyntaxNode {
        self.syntax
    }
}
impl std::fmt::Debug for JavaClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("JavaClass")
            .field(
                "class_token",
                &support::DebugSyntaxResult(self.class_token()),
            )
            .field("name_token", &support::DebugSyntaxResult(self.name_token()))
            .field("body", &support::DebugSyntaxResult(self.body()))
            .finish()
    }
}
impl From<JavaClass> for SyntaxNode {
    fn from(n: JavaClass) -> SyntaxNode {
        n.syntax
    }
}
impl From<JavaClass> for SyntaxElement {
    fn from(n: JavaClass) -> SyntaxElement {
        n.syntax.into()
    }
}
impl AstNode for JavaClassBody {
    type Language = Language;
    const KIND_SET: SyntaxKindSet<Language> =
        SyntaxKindSet::from_raw(RawSyntaxKind(JAVA_CLASS_BODY as u16));
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == JAVA_CLASS_BODY
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
    fn into_syntax(self) -> SyntaxNode {
        self.syntax
    }
}
impl std::fmt::Debug for JavaClassBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("JavaClassBody")
            .field(
                "l_curly_token",
                &support::DebugSyntaxResult(self.l_curly_token()),
            )
            .field("members", &self.members())
            .field(
                "r_curly_token",
                &support::DebugSyntaxResult(self.r_curly_token()),
            )
            .finish()
    }
}
impl From<JavaClassBody> for SyntaxNode {
    fn from(n: JavaClassBody) -> SyntaxNode {
        n.syntax
    }
}
impl From<JavaClassBody> for SyntaxElement {
    fn from(n: JavaClassBody) -> SyntaxElement {
        n.syntax.into()
    }
}
impl AstNode for JavaExpressionStatement {
    type Language = Language;
    const KIND_SET: SyntaxKindSet<Language> =
        SyntaxKindSet::from_raw(RawSyntaxKind(JAVA_EXPRESSION_STATEMENT as u16));
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == JAVA_EXPRESSION_STATEMENT
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
    fn into_syntax(self) -> SyntaxNode {
        self.syntax
    }
}
impl std::fmt::Debug for JavaExpressionStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("JavaExpressionStatement")
            .field(
                "expression_token",
                &support::DebugSyntaxResult(self.expression_token()),
            )
            .field(
                "semicolon_token",
                &support::DebugSyntaxResult(self.semicolon_token()),
            )
            .finish()
    }
}
impl From<JavaExpressionStatement> for SyntaxNode {
    fn from(n: JavaExpressionStatement) -> SyntaxNode {
        n.syntax
    }
}
impl From<JavaExpressionStatement> for SyntaxElement {
    fn from(n: JavaExpressionStatement) -> SyntaxElement {
        n.syntax.into()
    }
}
impl AstNode for JavaField {
    type Language = Language;
    const KIND_SET: SyntaxKindSet<Language> =
        SyntaxKindSet::from_raw(RawSyntaxKind(JAVA_FIELD as u16));
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == JAVA_FIELD
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
    fn into_syntax(self) -> SyntaxNode {
        self.syntax
    }
}
impl std::fmt::Debug for JavaField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("JavaField")
            .field(
                "field_token",
                &support::DebugSyntaxResult(self.field_token()),
            )
            .field("name_token", &support::DebugSyntaxResult(self.name_token()))
            .field("type_token", &support::DebugSyntaxResult(self.type_token()))
            .field(
                "semicolon_token",
                &support::DebugSyntaxResult(self.semicolon_token()),
            )
            .finish()
    }
}
impl From<JavaField> for SyntaxNode {
    fn from(n: JavaField) -> SyntaxNode {
        n.syntax
    }
}
impl From<JavaField> for SyntaxElement {
    fn from(n: JavaField) -> SyntaxElement {
        n.syntax.into()
    }
}
impl AstNode for JavaImport {
    type Language = Language;
    const KIND_SET: SyntaxKindSet<Language> =
        SyntaxKindSet::from_raw(RawSyntaxKind(JAVA_IMPORT as u16));
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == JAVA_IMPORT
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
    fn into_syntax(self) -> SyntaxNode {
        self.syntax
    }
}
impl std::fmt::Debug for JavaImport {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("JavaImport")
            .field(
                "import_token",
                &support::DebugSyntaxResult(self.import_token()),
            )
            .field("name_token", &support::DebugSyntaxResult(self.name_token()))
            .field(
                "semicolon_token",
                &support::DebugSyntaxResult(self.semicolon_token()),
            )
            .finish()
    }
}
impl From<JavaImport> for SyntaxNode {
    fn from(n: JavaImport) -> SyntaxNode {
        n.syntax
    }
}
impl From<JavaImport> for SyntaxElement {
    fn from(n: JavaImport) -> SyntaxElement {
        n.syntax.into()
    }
}
impl AstNode for JavaLiteral {
    type Language = Language;
    const KIND_SET: SyntaxKindSet<Language> =
        SyntaxKindSet::from_raw(RawSyntaxKind(JAVA_LITERAL as u16));
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == JAVA_LITERAL
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
    fn into_syntax(self) -> SyntaxNode {
        self.syntax
    }
}
impl std::fmt::Debug for JavaLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("JavaLiteral")
            .field(
                "literal_token",
                &support::DebugSyntaxResult(self.literal_token()),
            )
            .field(
                "value_token",
                &support::DebugSyntaxResult(self.value_token()),
            )
            .field(
                "semicolon_token",
                &support::DebugSyntaxResult(self.semicolon_token()),
            )
            .finish()
    }
}
impl From<JavaLiteral> for SyntaxNode {
    fn from(n: JavaLiteral) -> SyntaxNode {
        n.syntax
    }
}
impl From<JavaLiteral> for SyntaxElement {
    fn from(n: JavaLiteral) -> SyntaxElement {
        n.syntax.into()
    }
}
impl AstNode for JavaMethod {
    type Language = Language;
    const KIND_SET: SyntaxKindSet<Language> =
        SyntaxKindSet::from_raw(RawSyntaxKind(JAVA_METHOD as u16));
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == JAVA_METHOD
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
    fn into_syntax(self) -> SyntaxNode {
        self.syntax
    }
}
impl std::fmt::Debug for JavaMethod {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("JavaMethod")
            .field(
                "method_token",
                &support::DebugSyntaxResult(self.method_token()),
            )
            .field("name_token", &support::DebugSyntaxResult(self.name_token()))
            .field(
                "returnType_token",
                &support::DebugSyntaxResult(self.returnType_token()),
            )
            .field("body", &support::DebugSyntaxResult(self.body()))
            .finish()
    }
}
impl From<JavaMethod> for SyntaxNode {
    fn from(n: JavaMethod) -> SyntaxNode {
        n.syntax
    }
}
impl From<JavaMethod> for SyntaxElement {
    fn from(n: JavaMethod) -> SyntaxElement {
        n.syntax.into()
    }
}
impl AstNode for JavaMethodBody {
    type Language = Language;
    const KIND_SET: SyntaxKindSet<Language> =
        SyntaxKindSet::from_raw(RawSyntaxKind(JAVA_METHOD_BODY as u16));
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == JAVA_METHOD_BODY
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
    fn into_syntax(self) -> SyntaxNode {
        self.syntax
    }
}
impl std::fmt::Debug for JavaMethodBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("JavaMethodBody")
            .field(
                "l_curly_token",
                &support::DebugSyntaxResult(self.l_curly_token()),
            )
            .field("statements", &self.statements())
            .field(
                "r_curly_token",
                &support::DebugSyntaxResult(self.r_curly_token()),
            )
            .finish()
    }
}
impl From<JavaMethodBody> for SyntaxNode {
    fn from(n: JavaMethodBody) -> SyntaxNode {
        n.syntax
    }
}
impl From<JavaMethodBody> for SyntaxElement {
    fn from(n: JavaMethodBody) -> SyntaxElement {
        n.syntax.into()
    }
}
impl AstNode for JavaMethodCall {
    type Language = Language;
    const KIND_SET: SyntaxKindSet<Language> =
        SyntaxKindSet::from_raw(RawSyntaxKind(JAVA_METHOD_CALL as u16));
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == JAVA_METHOD_CALL
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
    fn into_syntax(self) -> SyntaxNode {
        self.syntax
    }
}
impl std::fmt::Debug for JavaMethodCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("JavaMethodCall")
            .field(
                "methodCall_token",
                &support::DebugSyntaxResult(self.methodCall_token()),
            )
            .field("name_token", &support::DebugSyntaxResult(self.name_token()))
            .field(
                "l_paren_token",
                &support::DebugSyntaxResult(self.l_paren_token()),
            )
            .field("arguments", &self.arguments())
            .field(
                "r_paren_token",
                &support::DebugSyntaxResult(self.r_paren_token()),
            )
            .field(
                "semicolon_token",
                &support::DebugSyntaxResult(self.semicolon_token()),
            )
            .finish()
    }
}
impl From<JavaMethodCall> for SyntaxNode {
    fn from(n: JavaMethodCall) -> SyntaxNode {
        n.syntax
    }
}
impl From<JavaMethodCall> for SyntaxElement {
    fn from(n: JavaMethodCall) -> SyntaxElement {
        n.syntax.into()
    }
}
impl AstNode for JavaPackage {
    type Language = Language;
    const KIND_SET: SyntaxKindSet<Language> =
        SyntaxKindSet::from_raw(RawSyntaxKind(JAVA_PACKAGE as u16));
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == JAVA_PACKAGE
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
    fn into_syntax(self) -> SyntaxNode {
        self.syntax
    }
}
impl std::fmt::Debug for JavaPackage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("JavaPackage")
            .field(
                "package_token",
                &support::DebugSyntaxResult(self.package_token()),
            )
            .field("name_token", &support::DebugSyntaxResult(self.name_token()))
            .field(
                "semicolon_token",
                &support::DebugSyntaxResult(self.semicolon_token()),
            )
            .finish()
    }
}
impl From<JavaPackage> for SyntaxNode {
    fn from(n: JavaPackage) -> SyntaxNode {
        n.syntax
    }
}
impl From<JavaPackage> for SyntaxElement {
    fn from(n: JavaPackage) -> SyntaxElement {
        n.syntax.into()
    }
}
impl AstNode for JavaReturnStatement {
    type Language = Language;
    const KIND_SET: SyntaxKindSet<Language> =
        SyntaxKindSet::from_raw(RawSyntaxKind(JAVA_RETURN_STATEMENT as u16));
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == JAVA_RETURN_STATEMENT
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
    fn into_syntax(self) -> SyntaxNode {
        self.syntax
    }
}
impl std::fmt::Debug for JavaReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("JavaReturnStatement")
            .field(
                "return_token",
                &support::DebugSyntaxResult(self.return_token()),
            )
            .field("expression", &support::DebugSyntaxResult(self.expression()))
            .field(
                "semicolon_token",
                &support::DebugSyntaxResult(self.semicolon_token()),
            )
            .finish()
    }
}
impl From<JavaReturnStatement> for SyntaxNode {
    fn from(n: JavaReturnStatement) -> SyntaxNode {
        n.syntax
    }
}
impl From<JavaReturnStatement> for SyntaxElement {
    fn from(n: JavaReturnStatement) -> SyntaxElement {
        n.syntax.into()
    }
}
impl AstNode for JavaRoot {
    type Language = Language;
    const KIND_SET: SyntaxKindSet<Language> =
        SyntaxKindSet::from_raw(RawSyntaxKind(JAVA_ROOT as u16));
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == JAVA_ROOT
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
    fn into_syntax(self) -> SyntaxNode {
        self.syntax
    }
}
impl std::fmt::Debug for JavaRoot {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("JavaRoot")
            .field(
                "bom_token",
                &support::DebugOptionalElement(self.bom_token()),
            )
            .field("package", &support::DebugSyntaxResult(self.package()))
            .field("imports", &self.imports())
            .field("classes", &self.classes())
            .field("eof_token", &support::DebugSyntaxResult(self.eof_token()))
            .finish()
    }
}
impl From<JavaRoot> for SyntaxNode {
    fn from(n: JavaRoot) -> SyntaxNode {
        n.syntax
    }
}
impl From<JavaRoot> for SyntaxElement {
    fn from(n: JavaRoot) -> SyntaxElement {
        n.syntax.into()
    }
}
impl AstNode for JavaVariable {
    type Language = Language;
    const KIND_SET: SyntaxKindSet<Language> =
        SyntaxKindSet::from_raw(RawSyntaxKind(JAVA_VARIABLE as u16));
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == JAVA_VARIABLE
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
    fn into_syntax(self) -> SyntaxNode {
        self.syntax
    }
}
impl std::fmt::Debug for JavaVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("JavaVariable")
            .field(
                "variable_token",
                &support::DebugSyntaxResult(self.variable_token()),
            )
            .field("name_token", &support::DebugSyntaxResult(self.name_token()))
            .field(
                "semicolon_token",
                &support::DebugSyntaxResult(self.semicolon_token()),
            )
            .finish()
    }
}
impl From<JavaVariable> for SyntaxNode {
    fn from(n: JavaVariable) -> SyntaxNode {
        n.syntax
    }
}
impl From<JavaVariable> for SyntaxElement {
    fn from(n: JavaVariable) -> SyntaxElement {
        n.syntax.into()
    }
}
impl From<JavaField> for JavaClassMember {
    fn from(node: JavaField) -> JavaClassMember {
        JavaClassMember::JavaField(node)
    }
}
impl From<JavaMethod> for JavaClassMember {
    fn from(node: JavaMethod) -> JavaClassMember {
        JavaClassMember::JavaMethod(node)
    }
}
impl AstNode for JavaClassMember {
    type Language = Language;
    const KIND_SET: SyntaxKindSet<Language> = JavaField::KIND_SET.union(JavaMethod::KIND_SET);
    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(kind, JAVA_FIELD | JAVA_METHOD)
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        let res = match syntax.kind() {
            JAVA_FIELD => JavaClassMember::JavaField(JavaField { syntax }),
            JAVA_METHOD => JavaClassMember::JavaMethod(JavaMethod { syntax }),
            _ => return None,
        };
        Some(res)
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            JavaClassMember::JavaField(it) => &it.syntax,
            JavaClassMember::JavaMethod(it) => &it.syntax,
        }
    }
    fn into_syntax(self) -> SyntaxNode {
        match self {
            JavaClassMember::JavaField(it) => it.syntax,
            JavaClassMember::JavaMethod(it) => it.syntax,
        }
    }
}
impl std::fmt::Debug for JavaClassMember {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JavaClassMember::JavaField(it) => std::fmt::Debug::fmt(it, f),
            JavaClassMember::JavaMethod(it) => std::fmt::Debug::fmt(it, f),
        }
    }
}
impl From<JavaClassMember> for SyntaxNode {
    fn from(n: JavaClassMember) -> SyntaxNode {
        match n {
            JavaClassMember::JavaField(it) => it.into(),
            JavaClassMember::JavaMethod(it) => it.into(),
        }
    }
}
impl From<JavaClassMember> for SyntaxElement {
    fn from(n: JavaClassMember) -> SyntaxElement {
        let node: SyntaxNode = n.into();
        node.into()
    }
}
impl From<JavaLiteral> for JavaExpression {
    fn from(node: JavaLiteral) -> JavaExpression {
        JavaExpression::JavaLiteral(node)
    }
}
impl From<JavaMethodCall> for JavaExpression {
    fn from(node: JavaMethodCall) -> JavaExpression {
        JavaExpression::JavaMethodCall(node)
    }
}
impl From<JavaVariable> for JavaExpression {
    fn from(node: JavaVariable) -> JavaExpression {
        JavaExpression::JavaVariable(node)
    }
}
impl AstNode for JavaExpression {
    type Language = Language;
    const KIND_SET: SyntaxKindSet<Language> = JavaLiteral::KIND_SET
        .union(JavaMethodCall::KIND_SET)
        .union(JavaVariable::KIND_SET);
    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(kind, JAVA_LITERAL | JAVA_METHOD_CALL | JAVA_VARIABLE)
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        let res = match syntax.kind() {
            JAVA_LITERAL => JavaExpression::JavaLiteral(JavaLiteral { syntax }),
            JAVA_METHOD_CALL => JavaExpression::JavaMethodCall(JavaMethodCall { syntax }),
            JAVA_VARIABLE => JavaExpression::JavaVariable(JavaVariable { syntax }),
            _ => return None,
        };
        Some(res)
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            JavaExpression::JavaLiteral(it) => &it.syntax,
            JavaExpression::JavaMethodCall(it) => &it.syntax,
            JavaExpression::JavaVariable(it) => &it.syntax,
        }
    }
    fn into_syntax(self) -> SyntaxNode {
        match self {
            JavaExpression::JavaLiteral(it) => it.syntax,
            JavaExpression::JavaMethodCall(it) => it.syntax,
            JavaExpression::JavaVariable(it) => it.syntax,
        }
    }
}
impl std::fmt::Debug for JavaExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JavaExpression::JavaLiteral(it) => std::fmt::Debug::fmt(it, f),
            JavaExpression::JavaMethodCall(it) => std::fmt::Debug::fmt(it, f),
            JavaExpression::JavaVariable(it) => std::fmt::Debug::fmt(it, f),
        }
    }
}
impl From<JavaExpression> for SyntaxNode {
    fn from(n: JavaExpression) -> SyntaxNode {
        match n {
            JavaExpression::JavaLiteral(it) => it.into(),
            JavaExpression::JavaMethodCall(it) => it.into(),
            JavaExpression::JavaVariable(it) => it.into(),
        }
    }
}
impl From<JavaExpression> for SyntaxElement {
    fn from(n: JavaExpression) -> SyntaxElement {
        let node: SyntaxNode = n.into();
        node.into()
    }
}
impl From<JavaExpressionStatement> for JavaStatement {
    fn from(node: JavaExpressionStatement) -> JavaStatement {
        JavaStatement::JavaExpressionStatement(node)
    }
}
impl From<JavaReturnStatement> for JavaStatement {
    fn from(node: JavaReturnStatement) -> JavaStatement {
        JavaStatement::JavaReturnStatement(node)
    }
}
impl AstNode for JavaStatement {
    type Language = Language;
    const KIND_SET: SyntaxKindSet<Language> =
        JavaExpressionStatement::KIND_SET.union(JavaReturnStatement::KIND_SET);
    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(kind, JAVA_EXPRESSION_STATEMENT | JAVA_RETURN_STATEMENT)
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        let res = match syntax.kind() {
            JAVA_EXPRESSION_STATEMENT => {
                JavaStatement::JavaExpressionStatement(JavaExpressionStatement { syntax })
            }
            JAVA_RETURN_STATEMENT => {
                JavaStatement::JavaReturnStatement(JavaReturnStatement { syntax })
            }
            _ => return None,
        };
        Some(res)
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            JavaStatement::JavaExpressionStatement(it) => &it.syntax,
            JavaStatement::JavaReturnStatement(it) => &it.syntax,
        }
    }
    fn into_syntax(self) -> SyntaxNode {
        match self {
            JavaStatement::JavaExpressionStatement(it) => it.syntax,
            JavaStatement::JavaReturnStatement(it) => it.syntax,
        }
    }
}
impl std::fmt::Debug for JavaStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JavaStatement::JavaExpressionStatement(it) => std::fmt::Debug::fmt(it, f),
            JavaStatement::JavaReturnStatement(it) => std::fmt::Debug::fmt(it, f),
        }
    }
}
impl From<JavaStatement> for SyntaxNode {
    fn from(n: JavaStatement) -> SyntaxNode {
        match n {
            JavaStatement::JavaExpressionStatement(it) => it.into(),
            JavaStatement::JavaReturnStatement(it) => it.into(),
        }
    }
}
impl From<JavaStatement> for SyntaxElement {
    fn from(n: JavaStatement) -> SyntaxElement {
        let node: SyntaxNode = n.into();
        node.into()
    }
}
impl std::fmt::Display for JavaClassMember {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for JavaExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for JavaStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for JavaClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for JavaClassBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for JavaExpressionStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for JavaField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for JavaImport {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for JavaLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for JavaMethod {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for JavaMethodBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for JavaMethodCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for JavaPackage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for JavaReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for JavaRoot {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for JavaVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[derive(Clone, PartialEq, Eq, Hash, Serialize)]
pub struct JavaBogus {
    syntax: SyntaxNode,
}
impl JavaBogus {
    #[doc = r" Create an AstNode from a SyntaxNode without checking its kind"]
    #[doc = r""]
    #[doc = r" # Safety"]
    #[doc = r" This function must be guarded with a call to [AstNode::can_cast]"]
    #[doc = r" or a match on [SyntaxNode::kind]"]
    #[inline]
    pub const unsafe fn new_unchecked(syntax: SyntaxNode) -> Self {
        Self { syntax }
    }
    pub fn items(&self) -> SyntaxElementChildren {
        support::elements(&self.syntax)
    }
}
impl AstNode for JavaBogus {
    type Language = Language;
    const KIND_SET: SyntaxKindSet<Language> =
        SyntaxKindSet::from_raw(RawSyntaxKind(JAVA_BOGUS as u16));
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == JAVA_BOGUS
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
    fn into_syntax(self) -> SyntaxNode {
        self.syntax
    }
}
impl std::fmt::Debug for JavaBogus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("JavaBogus")
            .field("items", &DebugSyntaxElementChildren(self.items()))
            .finish()
    }
}
impl From<JavaBogus> for SyntaxNode {
    fn from(n: JavaBogus) -> SyntaxNode {
        n.syntax
    }
}
impl From<JavaBogus> for SyntaxElement {
    fn from(n: JavaBogus) -> SyntaxElement {
        n.syntax.into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash, Serialize)]
pub struct JavaBogusValue {
    syntax: SyntaxNode,
}
impl JavaBogusValue {
    #[doc = r" Create an AstNode from a SyntaxNode without checking its kind"]
    #[doc = r""]
    #[doc = r" # Safety"]
    #[doc = r" This function must be guarded with a call to [AstNode::can_cast]"]
    #[doc = r" or a match on [SyntaxNode::kind]"]
    #[inline]
    pub const unsafe fn new_unchecked(syntax: SyntaxNode) -> Self {
        Self { syntax }
    }
    pub fn items(&self) -> SyntaxElementChildren {
        support::elements(&self.syntax)
    }
}
impl AstNode for JavaBogusValue {
    type Language = Language;
    const KIND_SET: SyntaxKindSet<Language> =
        SyntaxKindSet::from_raw(RawSyntaxKind(JAVA_BOGUS_VALUE as u16));
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == JAVA_BOGUS_VALUE
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
    fn into_syntax(self) -> SyntaxNode {
        self.syntax
    }
}
impl std::fmt::Debug for JavaBogusValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("JavaBogusValue")
            .field("items", &DebugSyntaxElementChildren(self.items()))
            .finish()
    }
}
impl From<JavaBogusValue> for SyntaxNode {
    fn from(n: JavaBogusValue) -> SyntaxNode {
        n.syntax
    }
}
impl From<JavaBogusValue> for SyntaxElement {
    fn from(n: JavaBogusValue) -> SyntaxElement {
        n.syntax.into()
    }
}
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct JavaClassList {
    syntax_list: SyntaxList,
}
impl JavaClassList {
    #[doc = r" Create an AstNode from a SyntaxNode without checking its kind"]
    #[doc = r""]
    #[doc = r" # Safety"]
    #[doc = r" This function must be guarded with a call to [AstNode::can_cast]"]
    #[doc = r" or a match on [SyntaxNode::kind]"]
    #[inline]
    pub unsafe fn new_unchecked(syntax: SyntaxNode) -> Self {
        Self {
            syntax_list: syntax.into_list(),
        }
    }
}
impl AstNode for JavaClassList {
    type Language = Language;
    const KIND_SET: SyntaxKindSet<Language> =
        SyntaxKindSet::from_raw(RawSyntaxKind(JAVA_CLASS_LIST as u16));
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == JAVA_CLASS_LIST
    }
    fn cast(syntax: SyntaxNode) -> Option<JavaClassList> {
        if Self::can_cast(syntax.kind()) {
            Some(JavaClassList {
                syntax_list: syntax.into_list(),
            })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        self.syntax_list.node()
    }
    fn into_syntax(self) -> SyntaxNode {
        self.syntax_list.into_node()
    }
}
impl Serialize for JavaClassList {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(self.len()))?;
        for e in self.iter() {
            seq.serialize_element(&e)?;
        }
        seq.end()
    }
}
impl AstNodeList for JavaClassList {
    type Language = Language;
    type Node = JavaClass;
    fn syntax_list(&self) -> &SyntaxList {
        &self.syntax_list
    }
    fn into_syntax_list(self) -> SyntaxList {
        self.syntax_list
    }
}
impl Debug for JavaClassList {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("JavaClassList ")?;
        f.debug_list().entries(self.iter()).finish()
    }
}
impl IntoIterator for &JavaClassList {
    type Item = JavaClass;
    type IntoIter = AstNodeListIterator<Language, JavaClass>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}
impl IntoIterator for JavaClassList {
    type Item = JavaClass;
    type IntoIter = AstNodeListIterator<Language, JavaClass>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct JavaClassMemberList {
    syntax_list: SyntaxList,
}
impl JavaClassMemberList {
    #[doc = r" Create an AstNode from a SyntaxNode without checking its kind"]
    #[doc = r""]
    #[doc = r" # Safety"]
    #[doc = r" This function must be guarded with a call to [AstNode::can_cast]"]
    #[doc = r" or a match on [SyntaxNode::kind]"]
    #[inline]
    pub unsafe fn new_unchecked(syntax: SyntaxNode) -> Self {
        Self {
            syntax_list: syntax.into_list(),
        }
    }
}
impl AstNode for JavaClassMemberList {
    type Language = Language;
    const KIND_SET: SyntaxKindSet<Language> =
        SyntaxKindSet::from_raw(RawSyntaxKind(JAVA_CLASS_MEMBER_LIST as u16));
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == JAVA_CLASS_MEMBER_LIST
    }
    fn cast(syntax: SyntaxNode) -> Option<JavaClassMemberList> {
        if Self::can_cast(syntax.kind()) {
            Some(JavaClassMemberList {
                syntax_list: syntax.into_list(),
            })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        self.syntax_list.node()
    }
    fn into_syntax(self) -> SyntaxNode {
        self.syntax_list.into_node()
    }
}
impl Serialize for JavaClassMemberList {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(self.len()))?;
        for e in self.iter() {
            seq.serialize_element(&e)?;
        }
        seq.end()
    }
}
impl AstNodeList for JavaClassMemberList {
    type Language = Language;
    type Node = JavaClassMember;
    fn syntax_list(&self) -> &SyntaxList {
        &self.syntax_list
    }
    fn into_syntax_list(self) -> SyntaxList {
        self.syntax_list
    }
}
impl Debug for JavaClassMemberList {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("JavaClassMemberList ")?;
        f.debug_list().entries(self.iter()).finish()
    }
}
impl IntoIterator for &JavaClassMemberList {
    type Item = JavaClassMember;
    type IntoIter = AstNodeListIterator<Language, JavaClassMember>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}
impl IntoIterator for JavaClassMemberList {
    type Item = JavaClassMember;
    type IntoIter = AstNodeListIterator<Language, JavaClassMember>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct JavaExpressionList {
    syntax_list: SyntaxList,
}
impl JavaExpressionList {
    #[doc = r" Create an AstNode from a SyntaxNode without checking its kind"]
    #[doc = r""]
    #[doc = r" # Safety"]
    #[doc = r" This function must be guarded with a call to [AstNode::can_cast]"]
    #[doc = r" or a match on [SyntaxNode::kind]"]
    #[inline]
    pub unsafe fn new_unchecked(syntax: SyntaxNode) -> Self {
        Self {
            syntax_list: syntax.into_list(),
        }
    }
}
impl AstNode for JavaExpressionList {
    type Language = Language;
    const KIND_SET: SyntaxKindSet<Language> =
        SyntaxKindSet::from_raw(RawSyntaxKind(JAVA_EXPRESSION_LIST as u16));
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == JAVA_EXPRESSION_LIST
    }
    fn cast(syntax: SyntaxNode) -> Option<JavaExpressionList> {
        if Self::can_cast(syntax.kind()) {
            Some(JavaExpressionList {
                syntax_list: syntax.into_list(),
            })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        self.syntax_list.node()
    }
    fn into_syntax(self) -> SyntaxNode {
        self.syntax_list.into_node()
    }
}
impl Serialize for JavaExpressionList {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(self.len()))?;
        for e in self.iter() {
            seq.serialize_element(&e)?;
        }
        seq.end()
    }
}
impl AstSeparatedList for JavaExpressionList {
    type Language = Language;
    type Node = JavaExpression;
    fn syntax_list(&self) -> &SyntaxList {
        &self.syntax_list
    }
    fn into_syntax_list(self) -> SyntaxList {
        self.syntax_list
    }
}
impl Debug for JavaExpressionList {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("JavaExpressionList ")?;
        f.debug_list().entries(self.elements()).finish()
    }
}
impl IntoIterator for JavaExpressionList {
    type Item = SyntaxResult<JavaExpression>;
    type IntoIter = AstSeparatedListNodesIterator<Language, JavaExpression>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}
impl IntoIterator for &JavaExpressionList {
    type Item = SyntaxResult<JavaExpression>;
    type IntoIter = AstSeparatedListNodesIterator<Language, JavaExpression>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct JavaImportList {
    syntax_list: SyntaxList,
}
impl JavaImportList {
    #[doc = r" Create an AstNode from a SyntaxNode without checking its kind"]
    #[doc = r""]
    #[doc = r" # Safety"]
    #[doc = r" This function must be guarded with a call to [AstNode::can_cast]"]
    #[doc = r" or a match on [SyntaxNode::kind]"]
    #[inline]
    pub unsafe fn new_unchecked(syntax: SyntaxNode) -> Self {
        Self {
            syntax_list: syntax.into_list(),
        }
    }
}
impl AstNode for JavaImportList {
    type Language = Language;
    const KIND_SET: SyntaxKindSet<Language> =
        SyntaxKindSet::from_raw(RawSyntaxKind(JAVA_IMPORT_LIST as u16));
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == JAVA_IMPORT_LIST
    }
    fn cast(syntax: SyntaxNode) -> Option<JavaImportList> {
        if Self::can_cast(syntax.kind()) {
            Some(JavaImportList {
                syntax_list: syntax.into_list(),
            })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        self.syntax_list.node()
    }
    fn into_syntax(self) -> SyntaxNode {
        self.syntax_list.into_node()
    }
}
impl Serialize for JavaImportList {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(self.len()))?;
        for e in self.iter() {
            seq.serialize_element(&e)?;
        }
        seq.end()
    }
}
impl AstNodeList for JavaImportList {
    type Language = Language;
    type Node = JavaImport;
    fn syntax_list(&self) -> &SyntaxList {
        &self.syntax_list
    }
    fn into_syntax_list(self) -> SyntaxList {
        self.syntax_list
    }
}
impl Debug for JavaImportList {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("JavaImportList ")?;
        f.debug_list().entries(self.iter()).finish()
    }
}
impl IntoIterator for &JavaImportList {
    type Item = JavaImport;
    type IntoIter = AstNodeListIterator<Language, JavaImport>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}
impl IntoIterator for JavaImportList {
    type Item = JavaImport;
    type IntoIter = AstNodeListIterator<Language, JavaImport>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct JavaStatementList {
    syntax_list: SyntaxList,
}
impl JavaStatementList {
    #[doc = r" Create an AstNode from a SyntaxNode without checking its kind"]
    #[doc = r""]
    #[doc = r" # Safety"]
    #[doc = r" This function must be guarded with a call to [AstNode::can_cast]"]
    #[doc = r" or a match on [SyntaxNode::kind]"]
    #[inline]
    pub unsafe fn new_unchecked(syntax: SyntaxNode) -> Self {
        Self {
            syntax_list: syntax.into_list(),
        }
    }
}
impl AstNode for JavaStatementList {
    type Language = Language;
    const KIND_SET: SyntaxKindSet<Language> =
        SyntaxKindSet::from_raw(RawSyntaxKind(JAVA_STATEMENT_LIST as u16));
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == JAVA_STATEMENT_LIST
    }
    fn cast(syntax: SyntaxNode) -> Option<JavaStatementList> {
        if Self::can_cast(syntax.kind()) {
            Some(JavaStatementList {
                syntax_list: syntax.into_list(),
            })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        self.syntax_list.node()
    }
    fn into_syntax(self) -> SyntaxNode {
        self.syntax_list.into_node()
    }
}
impl Serialize for JavaStatementList {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(self.len()))?;
        for e in self.iter() {
            seq.serialize_element(&e)?;
        }
        seq.end()
    }
}
impl AstNodeList for JavaStatementList {
    type Language = Language;
    type Node = JavaStatement;
    fn syntax_list(&self) -> &SyntaxList {
        &self.syntax_list
    }
    fn into_syntax_list(self) -> SyntaxList {
        self.syntax_list
    }
}
impl Debug for JavaStatementList {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("JavaStatementList ")?;
        f.debug_list().entries(self.iter()).finish()
    }
}
impl IntoIterator for &JavaStatementList {
    type Item = JavaStatement;
    type IntoIter = AstNodeListIterator<Language, JavaStatement>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}
impl IntoIterator for JavaStatementList {
    type Item = JavaStatement;
    type IntoIter = AstNodeListIterator<Language, JavaStatement>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}
#[derive(Clone)]
pub struct DebugSyntaxElementChildren(pub SyntaxElementChildren);
impl Debug for DebugSyntaxElementChildren {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_list()
            .entries(self.clone().0.map(DebugSyntaxElement))
            .finish()
    }
}
struct DebugSyntaxElement(SyntaxElement);
impl Debug for DebugSyntaxElement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.0 {
            SyntaxElement::Node(node) => {
                map_syntax_node ! (node . clone () , node => std :: fmt :: Debug :: fmt (& node , f))
            }
            SyntaxElement::Token(token) => Debug::fmt(token, f),
        }
    }
}
