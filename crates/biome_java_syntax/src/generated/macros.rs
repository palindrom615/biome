//! Generated file, do not edit by hand, see `xtask/codegen`

#[doc = r" Reconstruct an AstNode from a SyntaxNode"]
#[doc = r""]
#[doc = r" This macros performs a match over the [kind](biome_rowan::SyntaxNode::kind)"]
#[doc = r" of the provided [biome_rowan::SyntaxNode] and constructs the appropriate"]
#[doc = r" AstNode type for it, then execute the provided expression over it."]
#[doc = r""]
#[doc = r" # Examples"]
#[doc = r""]
#[doc = r" ```ignore"]
#[doc = r" map_syntax_node!(syntax_node, node => node.format())"]
#[doc = r" ```"]
#[macro_export]
macro_rules! map_syntax_node {
    ($ node : expr , $ pattern : pat => $ body : expr) => {
        match $node {
            node => match $crate::JavaSyntaxNode::kind(&node) {
                $crate::JavaSyntaxKind::JAVA_CLASS => {
                    let $pattern = unsafe { $crate::JavaClass::new_unchecked(node) };
                    $body
                }
                $crate::JavaSyntaxKind::JAVA_CLASS_BODY => {
                    let $pattern = unsafe { $crate::JavaClassBody::new_unchecked(node) };
                    $body
                }
                $crate::JavaSyntaxKind::JAVA_EXPRESSION_STATEMENT => {
                    let $pattern = unsafe { $crate::JavaExpressionStatement::new_unchecked(node) };
                    $body
                }
                $crate::JavaSyntaxKind::JAVA_FIELD => {
                    let $pattern = unsafe { $crate::JavaField::new_unchecked(node) };
                    $body
                }
                $crate::JavaSyntaxKind::JAVA_IMPORT => {
                    let $pattern = unsafe { $crate::JavaImport::new_unchecked(node) };
                    $body
                }
                $crate::JavaSyntaxKind::JAVA_LITERAL => {
                    let $pattern = unsafe { $crate::JavaLiteral::new_unchecked(node) };
                    $body
                }
                $crate::JavaSyntaxKind::JAVA_METHOD => {
                    let $pattern = unsafe { $crate::JavaMethod::new_unchecked(node) };
                    $body
                }
                $crate::JavaSyntaxKind::JAVA_METHOD_BODY => {
                    let $pattern = unsafe { $crate::JavaMethodBody::new_unchecked(node) };
                    $body
                }
                $crate::JavaSyntaxKind::JAVA_METHOD_CALL => {
                    let $pattern = unsafe { $crate::JavaMethodCall::new_unchecked(node) };
                    $body
                }
                $crate::JavaSyntaxKind::JAVA_PACKAGE => {
                    let $pattern = unsafe { $crate::JavaPackage::new_unchecked(node) };
                    $body
                }
                $crate::JavaSyntaxKind::JAVA_RETURN_STATEMENT => {
                    let $pattern = unsafe { $crate::JavaReturnStatement::new_unchecked(node) };
                    $body
                }
                $crate::JavaSyntaxKind::JAVA_ROOT => {
                    let $pattern = unsafe { $crate::JavaRoot::new_unchecked(node) };
                    $body
                }
                $crate::JavaSyntaxKind::JAVA_VARIABLE => {
                    let $pattern = unsafe { $crate::JavaVariable::new_unchecked(node) };
                    $body
                }
                $crate::JavaSyntaxKind::JAVA_BOGUS => {
                    let $pattern = unsafe { $crate::JavaBogus::new_unchecked(node) };
                    $body
                }
                $crate::JavaSyntaxKind::JAVA_BOGUS_VALUE => {
                    let $pattern = unsafe { $crate::JavaBogusValue::new_unchecked(node) };
                    $body
                }
                $crate::JavaSyntaxKind::JAVA_CLASS_LIST => {
                    let $pattern = unsafe { $crate::JavaClassList::new_unchecked(node) };
                    $body
                }
                $crate::JavaSyntaxKind::JAVA_CLASS_MEMBER_LIST => {
                    let $pattern = unsafe { $crate::JavaClassMemberList::new_unchecked(node) };
                    $body
                }
                $crate::JavaSyntaxKind::JAVA_EXPRESSION_LIST => {
                    let $pattern = unsafe { $crate::JavaExpressionList::new_unchecked(node) };
                    $body
                }
                $crate::JavaSyntaxKind::JAVA_IMPORT_LIST => {
                    let $pattern = unsafe { $crate::JavaImportList::new_unchecked(node) };
                    $body
                }
                $crate::JavaSyntaxKind::JAVA_STATEMENT_LIST => {
                    let $pattern = unsafe { $crate::JavaStatementList::new_unchecked(node) };
                    $body
                }
                _ => unreachable!(),
            },
        }
    };
}
pub(crate) use map_syntax_node;
