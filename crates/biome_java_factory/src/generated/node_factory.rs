//! Generated file, do not edit by hand, see `xtask/codegen`

#![allow(clippy::redundant_closure)]
#![allow(clippy::too_many_arguments)]
use biome_java_syntax::{
    JavaSyntaxElement as SyntaxElement, JavaSyntaxNode as SyntaxNode,
    JavaSyntaxToken as SyntaxToken, *,
};
use biome_rowan::AstNode;
pub fn java_class(
    class_token: SyntaxToken,
    name_token: SyntaxToken,
    body: JavaClassBody,
) -> JavaClass {
    JavaClass::unwrap_cast(SyntaxNode::new_detached(
        JavaSyntaxKind::JAVA_CLASS,
        [
            Some(SyntaxElement::Token(class_token)),
            Some(SyntaxElement::Token(name_token)),
            Some(SyntaxElement::Node(body.into_syntax())),
        ],
    ))
}
pub fn java_class_body(
    l_curly_token: SyntaxToken,
    members: JavaClassMemberList,
    r_curly_token: SyntaxToken,
) -> JavaClassBody {
    JavaClassBody::unwrap_cast(SyntaxNode::new_detached(
        JavaSyntaxKind::JAVA_CLASS_BODY,
        [
            Some(SyntaxElement::Token(l_curly_token)),
            Some(SyntaxElement::Node(members.into_syntax())),
            Some(SyntaxElement::Token(r_curly_token)),
        ],
    ))
}
pub fn java_expression_statement(
    expression_token: SyntaxToken,
    semicolon_token: SyntaxToken,
) -> JavaExpressionStatement {
    JavaExpressionStatement::unwrap_cast(SyntaxNode::new_detached(
        JavaSyntaxKind::JAVA_EXPRESSION_STATEMENT,
        [
            Some(SyntaxElement::Token(expression_token)),
            Some(SyntaxElement::Token(semicolon_token)),
        ],
    ))
}
pub fn java_field(
    field_token: SyntaxToken,
    name_token: SyntaxToken,
    type_token: SyntaxToken,
    semicolon_token: SyntaxToken,
) -> JavaField {
    JavaField::unwrap_cast(SyntaxNode::new_detached(
        JavaSyntaxKind::JAVA_FIELD,
        [
            Some(SyntaxElement::Token(field_token)),
            Some(SyntaxElement::Token(name_token)),
            Some(SyntaxElement::Token(type_token)),
            Some(SyntaxElement::Token(semicolon_token)),
        ],
    ))
}
pub fn java_import(
    import_token: SyntaxToken,
    name_token: SyntaxToken,
    semicolon_token: SyntaxToken,
) -> JavaImport {
    JavaImport::unwrap_cast(SyntaxNode::new_detached(
        JavaSyntaxKind::JAVA_IMPORT,
        [
            Some(SyntaxElement::Token(import_token)),
            Some(SyntaxElement::Token(name_token)),
            Some(SyntaxElement::Token(semicolon_token)),
        ],
    ))
}
pub fn java_literal(
    literal_token: SyntaxToken,
    value_token: SyntaxToken,
    semicolon_token: SyntaxToken,
) -> JavaLiteral {
    JavaLiteral::unwrap_cast(SyntaxNode::new_detached(
        JavaSyntaxKind::JAVA_LITERAL,
        [
            Some(SyntaxElement::Token(literal_token)),
            Some(SyntaxElement::Token(value_token)),
            Some(SyntaxElement::Token(semicolon_token)),
        ],
    ))
}
pub fn java_method(
    method_token: SyntaxToken,
    name_token: SyntaxToken,
    returnType_token: SyntaxToken,
    body: JavaMethodBody,
) -> JavaMethod {
    JavaMethod::unwrap_cast(SyntaxNode::new_detached(
        JavaSyntaxKind::JAVA_METHOD,
        [
            Some(SyntaxElement::Token(method_token)),
            Some(SyntaxElement::Token(name_token)),
            Some(SyntaxElement::Token(returnType_token)),
            Some(SyntaxElement::Node(body.into_syntax())),
        ],
    ))
}
pub fn java_method_body(
    l_curly_token: SyntaxToken,
    statements: JavaStatementList,
    r_curly_token: SyntaxToken,
) -> JavaMethodBody {
    JavaMethodBody::unwrap_cast(SyntaxNode::new_detached(
        JavaSyntaxKind::JAVA_METHOD_BODY,
        [
            Some(SyntaxElement::Token(l_curly_token)),
            Some(SyntaxElement::Node(statements.into_syntax())),
            Some(SyntaxElement::Token(r_curly_token)),
        ],
    ))
}
pub fn java_method_call(
    methodCall_token: SyntaxToken,
    name_token: SyntaxToken,
    l_paren_token: SyntaxToken,
    arguments: JavaExpressionList,
    r_paren_token: SyntaxToken,
    semicolon_token: SyntaxToken,
) -> JavaMethodCall {
    JavaMethodCall::unwrap_cast(SyntaxNode::new_detached(
        JavaSyntaxKind::JAVA_METHOD_CALL,
        [
            Some(SyntaxElement::Token(methodCall_token)),
            Some(SyntaxElement::Token(name_token)),
            Some(SyntaxElement::Token(l_paren_token)),
            Some(SyntaxElement::Node(arguments.into_syntax())),
            Some(SyntaxElement::Token(r_paren_token)),
            Some(SyntaxElement::Token(semicolon_token)),
        ],
    ))
}
pub fn java_package(
    package_token: SyntaxToken,
    name_token: SyntaxToken,
    semicolon_token: SyntaxToken,
) -> JavaPackage {
    JavaPackage::unwrap_cast(SyntaxNode::new_detached(
        JavaSyntaxKind::JAVA_PACKAGE,
        [
            Some(SyntaxElement::Token(package_token)),
            Some(SyntaxElement::Token(name_token)),
            Some(SyntaxElement::Token(semicolon_token)),
        ],
    ))
}
pub fn java_return_statement(
    return_token: SyntaxToken,
    expression: JavaExpression,
    semicolon_token: SyntaxToken,
) -> JavaReturnStatement {
    JavaReturnStatement::unwrap_cast(SyntaxNode::new_detached(
        JavaSyntaxKind::JAVA_RETURN_STATEMENT,
        [
            Some(SyntaxElement::Token(return_token)),
            Some(SyntaxElement::Node(expression.into_syntax())),
            Some(SyntaxElement::Token(semicolon_token)),
        ],
    ))
}
pub fn java_root(
    package: JavaPackage,
    imports: JavaImportList,
    classes: JavaClassList,
    eof_token: SyntaxToken,
) -> JavaRootBuilder {
    JavaRootBuilder {
        package,
        imports,
        classes,
        eof_token,
        bom_token: None,
    }
}
pub struct JavaRootBuilder {
    package: JavaPackage,
    imports: JavaImportList,
    classes: JavaClassList,
    eof_token: SyntaxToken,
    bom_token: Option<SyntaxToken>,
}
impl JavaRootBuilder {
    pub fn with_bom_token(mut self, bom_token: SyntaxToken) -> Self {
        self.bom_token = Some(bom_token);
        self
    }
    pub fn build(self) -> JavaRoot {
        JavaRoot::unwrap_cast(SyntaxNode::new_detached(
            JavaSyntaxKind::JAVA_ROOT,
            [
                self.bom_token.map(|token| SyntaxElement::Token(token)),
                Some(SyntaxElement::Node(self.package.into_syntax())),
                Some(SyntaxElement::Node(self.imports.into_syntax())),
                Some(SyntaxElement::Node(self.classes.into_syntax())),
                Some(SyntaxElement::Token(self.eof_token)),
            ],
        ))
    }
}
pub fn java_variable(
    variable_token: SyntaxToken,
    name_token: SyntaxToken,
    semicolon_token: SyntaxToken,
) -> JavaVariable {
    JavaVariable::unwrap_cast(SyntaxNode::new_detached(
        JavaSyntaxKind::JAVA_VARIABLE,
        [
            Some(SyntaxElement::Token(variable_token)),
            Some(SyntaxElement::Token(name_token)),
            Some(SyntaxElement::Token(semicolon_token)),
        ],
    ))
}
pub fn java_class_list<I>(items: I) -> JavaClassList
where
    I: IntoIterator<Item = JavaClass>,
    I::IntoIter: ExactSizeIterator,
{
    JavaClassList::unwrap_cast(SyntaxNode::new_detached(
        JavaSyntaxKind::JAVA_CLASS_LIST,
        items
            .into_iter()
            .map(|item| Some(item.into_syntax().into())),
    ))
}
pub fn java_class_member_list<I>(items: I) -> JavaClassMemberList
where
    I: IntoIterator<Item = JavaClassMember>,
    I::IntoIter: ExactSizeIterator,
{
    JavaClassMemberList::unwrap_cast(SyntaxNode::new_detached(
        JavaSyntaxKind::JAVA_CLASS_MEMBER_LIST,
        items
            .into_iter()
            .map(|item| Some(item.into_syntax().into())),
    ))
}
pub fn java_expression_list<I, S>(items: I, separators: S) -> JavaExpressionList
where
    I: IntoIterator<Item = JavaExpression>,
    I::IntoIter: ExactSizeIterator,
    S: IntoIterator<Item = JavaSyntaxToken>,
    S::IntoIter: ExactSizeIterator,
{
    let mut items = items.into_iter();
    let mut separators = separators.into_iter();
    let length = items.len() + separators.len();
    JavaExpressionList::unwrap_cast(SyntaxNode::new_detached(
        JavaSyntaxKind::JAVA_EXPRESSION_LIST,
        (0..length).map(|index| {
            if index % 2 == 0 {
                Some(items.next()?.into_syntax().into())
            } else {
                Some(separators.next()?.into())
            }
        }),
    ))
}
pub fn java_import_list<I>(items: I) -> JavaImportList
where
    I: IntoIterator<Item = JavaImport>,
    I::IntoIter: ExactSizeIterator,
{
    JavaImportList::unwrap_cast(SyntaxNode::new_detached(
        JavaSyntaxKind::JAVA_IMPORT_LIST,
        items
            .into_iter()
            .map(|item| Some(item.into_syntax().into())),
    ))
}
pub fn java_statement_list<I>(items: I) -> JavaStatementList
where
    I: IntoIterator<Item = JavaStatement>,
    I::IntoIter: ExactSizeIterator,
{
    JavaStatementList::unwrap_cast(SyntaxNode::new_detached(
        JavaSyntaxKind::JAVA_STATEMENT_LIST,
        items
            .into_iter()
            .map(|item| Some(item.into_syntax().into())),
    ))
}
pub fn java_bogus<I>(slots: I) -> JavaBogus
where
    I: IntoIterator<Item = Option<SyntaxElement>>,
    I::IntoIter: ExactSizeIterator,
{
    JavaBogus::unwrap_cast(SyntaxNode::new_detached(JavaSyntaxKind::JAVA_BOGUS, slots))
}
pub fn java_bogus_value<I>(slots: I) -> JavaBogusValue
where
    I: IntoIterator<Item = Option<SyntaxElement>>,
    I::IntoIter: ExactSizeIterator,
{
    JavaBogusValue::unwrap_cast(SyntaxNode::new_detached(
        JavaSyntaxKind::JAVA_BOGUS_VALUE,
        slots,
    ))
}
