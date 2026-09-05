from pygments.lexer import RegexLexer
from pygments.token import Comment, Keyword, Name, Number, Punctuation, String, Text


class YupianLexer(RegexLexer):
    name = "Yupian"
    aliases = ["yupian"]
    filenames = ["*.yupian"]
    tokens = {
        "root": [
            (r";.*$", Comment.Single),
            (r'"(\\.|[^"\\])*"', String),
            (r"[-+]?(?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+)", Number),
            (r"[()「」[]『』]", Punctuation),
            (r"(?:木|水|火|土|竹|十|戈|大|中|一|弓|人|心|手|口|尸|廿|山|田|難|卜)", Keyword),
            (r"[\w?!-]+", Name),
            (r"\s+", Text),
            (r".", Text),
        ]
    }
