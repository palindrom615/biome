use crate::kind_src::KindsSrc;

/// Java language kinds
///
/// ported from https://github.com/javaparser/javaparser/blob/master/javaparser-core/src/main/javacc/java.jj
pub const JAVA_KINDS_SRC: KindsSrc = KindsSrc {
    punct: &[
        (":", "COLON"),
        (",", "COMMA"),
        (".", "DOT"),
        ("(", "L_PAREN"),
        (")", "R_PAREN"),
        ("{", "L_CURLY"),
        ("}", "R_CURLY"),
        ("[", "L_BRACK"),
        ("]", "R_BRACK"),
        ("<", "L_ANGLE"),
        (">", "R_ANGLE"),
        ("~", "TILDE"),
        ("^", "CARET"),
        ("&", "AMP"),
        ("|", "PIPE"),
        ("+", "PLUS"),
        ("-", "MINUS"),
        ("/", "SLASH"),
        ("*", "STAR"),
        ("%", "PERCENT"),
        ("!", "BANG"),
        ("?", "QUESTION"),
        ("@", "AT"),
        ("<<", "SHL"),
        ("&", "AMP"),
        ("|", "PIPE"),
        ("=", "EQ"),
        ("==", "EQ2"),
        ("<=", "LTEQ"),
        (">=", "GTEQ"),
        ("+=", "PLUSEQ"),
        ("-=", "MINUSEQ"),
        ("|=", "PIPEEQ"),
        ("&=", "AMPEQ"),
        ("^=", "CARETEQ"),
        ("/=", "SLASHEQ"),
        ("*=", "STAREQ"),
        ("%=", "PERCENTEQ"),
        ("&&", "AMP2"),
        ("||", "PIPE2"),
        ("::", "COLON2"),
        ("++", "PLUS2"),
        ("--", "MINUS2"),
        ("->", "ARROW"),
    ],
    keywords: &[
        "abstract",
        "assert",
        "boolean",
        "break",
        "byte",
        "case",
        "catch",
        "char",
        "class",
        "const",
        "continue",
        "default",
        "do",
        "double",
        "else",
        "enum",
        "extends",
        "false",
        "final",
        "finally",
        "float",
        "for",
        "goto",
        "if",
        "implements",
        "import",
        "instanceof",
        "int",
        "interface",
        "long",
        "native",
        "new",
        "non-sealed",
        "null",
        "package",
        "permits",
        "private",
        "protected",
        "public",
        "record",
        "return",
        "sealed",
        "short",
        "static",
        "strictfp",
        "super",
        "switch",
        "synchronized",
        "this",
        "throw",
        "throws",
        "transient",
        "true",
        "try",
        "void",
        "volatile",
        "while",
        "yield",
        "requires",
        "to",
        "with",
        "open",
        "opens",
        "uses",
        "module",
        "exports",
        "provides",
        "transitive",
        "when",
    ],
    literals: &["JAVA_STRING_LITERAL", "JAVA_SCALAR"],
    tokens: &["NEWLINE", "WHITESPACE", "IDENT", "COMMENT"],
    nodes: &[
        "JAVA_ROOT",
        "JAVA_PACKAGE",
        "JAVA_IMPORT_LIST",
        "JAVA_IMPORT",
        "JAVA_CLASS_LIST",
        "JAVA_CLASS",
        // Bogus nodes
        "JAVA_BOGUS",
        "JAVA_BOGUS_VALUE",
    ],
};