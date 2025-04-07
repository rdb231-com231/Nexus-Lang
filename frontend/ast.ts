export type NodeType = 
    | "Program"
    | "NumericLiteral"
    | "Identifier"
    | "BinaryExpr";


export interface Stmt {
    kind: NodeType;
    start_pos: number;
    end_pos: number;
}

export interface Program extends Stmt {
    kind: "Program";
    body: Stmt[];
    start_pos: number;
    end_pos: number;
}

export interface Expr extends Stmt {}

export interface BinaryExpr extends Expr {
    kind: "BinaryExpr";
    left: Expr;
    right: Expr;
    operator: string;
    start_pos: number;
    end_pos: number;
}

export interface Identifier extends Expr {
    kind: "Identifier";
    symbol: string;
    start_pos: number;
    end_pos: number;
}

export interface NumericLiteral extends Expr {
    kind: "NumericLiteral";
    value: number;
    start_pos: number;
    end_pos: number;
}




