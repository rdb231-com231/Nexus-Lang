declare function require(name: string)
const Deno = require('deno')


export enum TokenType {
    // Literais
    Number,
    String,
    Identifier,

    // Keywords
    Definir,

    // Operators
    BinaryOperator, // +, -, /, *
    Equals, // = 
    OpenParen, // (
    CloseParen, // )
    EOF
}

const KEYWORDS: Record<string, TokenType> = {
    definir: TokenType.Definir
}

export interface Token {
    value: string; // valor básico do token
    type: TokenType; // tipo de token
    pos_start: number; // posição de começo
    pos_end: number; // posição de término
}


export function token (value = "", type: TokenType, pos_start: number, pos_end: number) {
    return { value, type, pos_start, pos_end } as Token
}


export class detect {
    isalpha(src: string) {
        return (src.toUpperCase() != src.toLowerCase())
    }
    
    isskippable(src: string) {
        return src == " " || src == "\n" || src == "\t"
    }

    isint(src: string) {
        const c = src.charCodeAt(0)
        const bounds = ["0".charCodeAt(0), "9".charCodeAt(0)];
        return c >= bounds[0] && c <= bounds[1];
    }
}

export function tokenize(sourceCode: string): Token[] {
    const tokens = new Array<Token>;
    const src = sourceCode.split("");
    let pos = 0
    const detector = new detect()
    while (src.length > 0) {
        if (src[0] == "(") {
            tokens.push(token(src.shift(), TokenType.OpenParen, pos, pos + 1));
            pos++;
        }
        else if (src[0] == ")") {
            tokens.push(token(src.shift(), TokenType.CloseParen, pos, pos + 1));
            pos++;
        }
        else if (src[0] == "+" || src[0] == "-" || src[0] == "*" || src[0] == "/") {
            tokens.push(token(src.shift(), TokenType.BinaryOperator, pos, pos + 1));
            pos++;
        }
        else if (src[0] == "=") {
            tokens.push(token(src.shift(), TokenType.Equals, pos, pos + 1));
            pos++;
        }
        else {
            if (detector.isint(src[0])) {
                let num = "";
                let start_pos = pos;
                while (src.length > 0 && detector.isint(src[0])) {
                    num += src.shift();
                    pos++;
                }

                tokens.push(token(num, TokenType.Number, start_pos, pos));
                pos++;
            }
            else if (detector.isalpha(src[0])) {
                let ident = "";
                let start_pos = pos;

                while (src.length > 0 && detector.isalpha(src[0])) {
                    ident += src.shift();
                    pos++;
                }
                
                
                const reserved = KEYWORDS[ident];
                if (reserved) {
                    tokens.push(token(ident, reserved, start_pos, pos))
                    pos++;
                } else {
                    tokens.push(token(ident, TokenType.Identifier, start_pos, pos))
                    pos++;
                }
            }
            else if (detector.isskippable(src[0])) {
                src.shift();
                pos++;
            }
            else {
                console.error(
                    `Unreconized character ${src[0]} at ${pos}`
                )
            }
            Deno.exit(1)
        }
    }

    return tokens;
}
