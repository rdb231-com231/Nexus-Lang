import {
    BinaryExpr,
    Expr,
    Identifier,
    NumericLiteral,
    Program,
    Stmt
} from "./ast.ts"

import {Token, TokenType, tokenize, detect} from "./lexer.ts"
const detector = new detect();

export default class Parser {
    private tokens: Token[] = [];

    private not_eof(): boolean {
        return this.tokens[0].type != TokenType.EOF;
    }
}
