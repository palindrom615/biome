//! Generated file, do not edit by hand, see `xtask/codegen`

#![allow(clippy::all)]
#![allow(bad_style, missing_docs, unreachable_pub)]
#[doc = r" The kind of syntax node, e.g. `IDENT`, `FUNCTION_KW`, or `FOR_STMT`."]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(u16)]
pub enum JavaSyntaxKind {
    #[doc(hidden)]
    TOMBSTONE,
    #[doc = r" Marks the end of the file. May have trivia attached"]
    EOF,
    #[doc = r" Any Unicode BOM character that may be present at the start of"]
    #[doc = r" a file."]
    UNICODE_BOM,
    COLON,
    COMMA,
    L_CURLY,
    R_CURLY,
    L_BRACK,
    R_BRACK,
    DASH,
    PERCENT,
    STAR,
    HASH,
    BANG,
    AT,
    SHL,
    AMP,
    PIPE,
    R_ANGLE,
    TILDE,
    BACKTICK,
    DOC_START,
    DOC_END,
    NULL_KW,
    JAVA_STRING_LITERAL,
    JAVA_SCALAR,
    NEWLINE,
    WHITESPACE,
    IDENT,
    COMMENT,
    JAVA_ROOT,
    JAVA_PACKAGE,
    JAVA_IMPORT_LIST,
    JAVA_IMPORT,
    JAVA_CLASS_LIST,
    JAVA_CLASS,
    JAVA_BOGUS,
    JAVA_BOGUS_VALUE,
    #[doc(hidden)]
    __LAST,
}
use self::JavaSyntaxKind::*;
impl JavaSyntaxKind {
    pub const fn is_punct(self) -> bool {
        match self {
            COLON | COMMA | L_CURLY | R_CURLY | L_BRACK | R_BRACK | DASH | PERCENT | STAR
            | HASH | BANG | AT | SHL | AMP | PIPE | R_ANGLE | TILDE | BACKTICK | DOC_START
            | DOC_END => true,
            _ => false,
        }
    }
    pub const fn is_literal(self) -> bool {
        match self {
            JAVA_STRING_LITERAL | JAVA_SCALAR => true,
            _ => false,
        }
    }
    pub const fn is_list(self) -> bool {
        match self {
            JAVA_IMPORT_LIST | JAVA_CLASS_LIST => true,
            _ => false,
        }
    }
    pub fn from_keyword(ident: &str) -> Option<JavaSyntaxKind> {
        let kw = match ident {
            "null" => NULL_KW,
            _ => return None,
        };
        Some(kw)
    }
    pub const fn to_string(&self) -> Option<&'static str> {
        let tok = match self {
            COLON => ":",
            COMMA => ",",
            L_CURLY => "{",
            R_CURLY => "}",
            L_BRACK => "[",
            R_BRACK => "]",
            DASH => "-",
            PERCENT => "%",
            STAR => "*",
            HASH => "#",
            BANG => "!",
            AT => "@",
            SHL => "<<",
            AMP => "&",
            PIPE => "|",
            R_ANGLE => ">",
            TILDE => "~",
            BACKTICK => "`",
            DOC_START => "---",
            DOC_END => "...",
            NULL_KW => "null",
            JAVA_STRING_LITERAL => "string literal",
            _ => return None,
        };
        Some(tok)
    }
}
#[doc = r" Utility macro for creating a SyntaxKind through simple macro syntax"]
#[macro_export]
macro_rules ! T { [:] => { $ crate :: JavaSyntaxKind :: COLON } ; [,] => { $ crate :: JavaSyntaxKind :: COMMA } ; ['{'] => { $ crate :: JavaSyntaxKind :: L_CURLY } ; ['}'] => { $ crate :: JavaSyntaxKind :: R_CURLY } ; ['['] => { $ crate :: JavaSyntaxKind :: L_BRACK } ; [']'] => { $ crate :: JavaSyntaxKind :: R_BRACK } ; [-] => { $ crate :: JavaSyntaxKind :: DASH } ; [%] => { $ crate :: JavaSyntaxKind :: PERCENT } ; [*] => { $ crate :: JavaSyntaxKind :: STAR } ; [#] => { $ crate :: JavaSyntaxKind :: HASH } ; [!] => { $ crate :: JavaSyntaxKind :: BANG } ; [@] => { $ crate :: JavaSyntaxKind :: AT } ; [<<] => { $ crate :: JavaSyntaxKind :: SHL } ; [&] => { $ crate :: JavaSyntaxKind :: AMP } ; [|] => { $ crate :: JavaSyntaxKind :: PIPE } ; [>] => { $ crate :: JavaSyntaxKind :: R_ANGLE } ; [~] => { $ crate :: JavaSyntaxKind :: TILDE } ; ['`'] => { $ crate :: JavaSyntaxKind :: BACKTICK } ; [---] => { $ crate :: JavaSyntaxKind :: DOC_START } ; [...] => { $ crate :: JavaSyntaxKind :: DOC_END } ; [null] => { $ crate :: JavaSyntaxKind :: NULL_KW } ; [ident] => { $ crate :: JavaSyntaxKind :: IDENT } ; [EOF] => { $ crate :: JavaSyntaxKind :: EOF } ; [UNICODE_BOM] => { $ crate :: JavaSyntaxKind :: UNICODE_BOM } ; [#] => { $ crate :: JavaSyntaxKind :: HASH } ; }
