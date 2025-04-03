from Interpreter import strings_with_arrows
import string
import os
from typing import Any

DIGITS = '0123456789'
LETTERS = string.ascii_letters
LETTERS_DIGITS = LETTERS + DIGITS

class Error:
	def __init__(self, pos_start, pos_end, error_name, details):
		self.pos_start = pos_start
		self.pos_end = pos_end
		self.error_name = error_name
		self.details = details

	def as_string(self):
		result = f'{self.error_name}: {self.details}\n'
		result += f'File {self.pos_start.fn}, line {self.pos_start.ln + 1}'
		result += '\n\n' + strings_with_arrows.string_with_arrows(self.pos_start.ftxt, self.pos_start, self.pos_end)
		return result

class IllegalCharError(Error):
	def __init__(self, pos_start, pos_end, details):
		super().__init__(pos_start, pos_end, 'Illegal Character', details)
	def __str__(self):
		return self.as_string()

class ExpectedCharError(Error):
	def __init__(self, pos_start, pos_end, details):
		super().__init__(pos_start, pos_end, 'Expected Character', details)
	def __str__(self):
		return self.as_string()

class InvalidSyntaxError(Error):
	def __init__(self, pos_start, pos_end, details=''):
		super().__init__(pos_start, pos_end, 'Invalid Syntax', details)
	def __str__(self):
		return self.as_string()

class CheckConditionError(Error):
	def __init__(self, pos_start, pos_end, details=''):
		super().__init__(pos_start, pos_end, 'Condition Value Expected', details)
	def __str__(self):
		return self.as_string()

class RTError(Error):
	def __init__(self, pos_start, pos_end, details, context):
		super().__init__(pos_start, pos_end, 'Runtime Error', details)
		self.context = context

	def as_string(self):
		result = self.generate_traceback()
		result += f'{self.error_name}: {self.details}\n'
		result += '\n\n' + strings_with_arrows.string_with_arrows(self.pos_start.ftxt, self.pos_start, self.pos_end)
		return result

	def generate_traceback(self):
		result = ''
		pos = self.pos_start
		ctx = self.context

		while ctx:
			result = f'  File {pos.fn}, line {str(pos.ln + 1)}, in {ctx.display_name}\n' + result
			pos = ctx.parent_entry_pos
			ctx = ctx.parent

		return 'Traceback (most recent call last):\n' + result
	
	def __str__(self):
		return self.as_string()

class Position:
	def __init__(self, idx, ln, col, fn, ftxt):
		self.idx = idx
		self.ln = ln
		self.col = col
		self.fn = fn
		self.ftxt = ftxt

	def advance(self, current_char=None):
		self.idx += 1
		self.col += 1

		if current_char == '\n':
			self.ln += 1
			self.col = 0

		return self

	def copy(self):
		return Position(self.idx, self.ln, self.col, self.fn, self.ftxt)

TT_INT = 'INT'
TT_FLOAT = 'FLOAT'
TT_IDENTIFIER = 'IDENTIFIER'
TT_STRING = 'STRING'
TT_KEYWORD = 'KEYWORD'
TT_PLUS = 'PLUS'
TT_MINUS = 'MINUS'
TT_MUL = 'MUL'
TT_DIV = 'DIV'
TT_POW = 'POW'
TT_EQ = 'EQ'
TT_LPAREN = 'LPAREN'
TT_RPAREN = 'RPAREN'
TT_RSQUARE = 'RSQUARE'
TT_LSQUARE = 'LSQUARE'
TT_EE = 'EE'
TT_NE = 'NE'
TT_LT = 'LT'
TT_GT = 'GT'
TT_LTE = 'LTE'
TT_GTE = 'GTE'
TT_EOF = 'EOF'
TT_COMMA = 'COMMA'
TT_NEWLINE = 'NEWLINE'
TT_ARROW = 'ARROW'
TT_COLON = 'COLON'
TT_RBRACE = 'RBRACE'
TT_LBRACE = 'LBRACE'

KEYWORDS = [
	'nexus',
	'and',
	'or',
	'not',
	'if',
	'elif',
	'else',
	'range',
	'to',
	'change',
	'while',
	'do',
	'def',
	'using',
	'end',
	'return',
	'continue',
	'break',
	'check',
	'raise',
	'as',
	'import',
	'from'
]

class Token:
	def __init__(self, type_, value=None, pos_start=None, pos_end=None):
		self.type = type_
		self.value = value
		self.pos_start = pos_start.copy() if pos_start else None
		self.pos_end = pos_end.copy() if pos_end else None
		
		if pos_start and not pos_end:
			self.pos_end = pos_start.copy()
			self.pos_end.advance()

	def matches(self, type_, value):
		return self.type == type_ and self.value == value
	
	def __repr__(self):
		if self.value: return f'{self.type}:{self.value}'
		return f'{self.type}'

class Lexer:
	def __init__(self, fn, text):
		self.fn = fn
		self.text = text
		self.pos = Position(-1, 0, -1, fn, text)
		self.current_char = None
		self.advance()
	
	def advance(self):
		self.pos.advance(self.current_char)
		self.current_char = self.text[self.pos.idx] if self.pos.idx < len(self.text) else None

	def make_tokens(self):
		tokens = []
		
		while self.current_char is not None:
			if self.current_char in ' \t':
				self.advance()
			elif self.current_char == '#':
				self.skip_comment()
			elif self.current_char in DIGITS:
				tokens.append(self.make_number())
			elif self.current_char in ';\n':
				tokens.append(Token(TT_NEWLINE, pos_start=self.pos))
				self.advance()
			elif self.current_char in LETTERS:
				tokens.append(self.make_identifier())
			elif self.current_char == '+':
				tokens.append(Token(TT_PLUS, pos_start=self.pos))
				self.advance()
			elif self.current_char == '-':
				tokens.append(Token(TT_MINUS, pos_start=self.pos))
				self.advance()
			elif self.current_char == '{':
				tokens.append(Token(TT_LBRACE, pos_start=self.pos)); self.advance()
			elif self.current_char == '}':
				tokens.append(Token(TT_RBRACE, pos_start=self.pos)); self.advance()
			elif self.current_char == ':':
				tokens.append(Token(TT_COLON, pos_start=self.pos)); self.advance()
			elif self.current_char == '"':
				token, error = self.make_string()
				if error: return [], error
				tokens.append(token)
			elif self.current_char == '*':
				tokens.append(Token(TT_MUL, pos_start=self.pos))
				self.advance()
			elif self.current_char == '/':
				tokens.append(Token(TT_DIV, pos_start=self.pos))
				self.advance()
			elif self.current_char == '^':
				tokens.append(Token(TT_POW, pos_start=self.pos))
				self.advance()
			elif self.current_char == '(':
				tokens.append(Token(TT_LPAREN, pos_start=self.pos))
				self.advance()
			elif self.current_char == ')':
				tokens.append(Token(TT_RPAREN, pos_start=self.pos))
				self.advance()
			elif self.current_char == '[':
				tokens.append(Token(TT_LSQUARE, pos_start=self.pos))
				self.advance()
			elif self.current_char == ']':
				tokens.append(Token(TT_RSQUARE, pos_start=self.pos))
				self.advance()
			elif self.current_char == '!':
				token, error = self.make_not_equals()
				if error: return [], error
				tokens.append(token)
			elif self.current_char == ',':
				tokens.append(Token(TT_COMMA, pos_start=self.pos))
				self.advance()
			elif self.current_char == '=':
				tokens.append(self.make_equals())
			elif self.current_char == '<':
				tokens.append(self.make_less_than())
			elif self.current_char == '>':
				tokens.append(self.make_greater_than())
			else:
				pos_start = self.pos.copy()
				char = self.current_char
				self.advance()
				return [], IllegalCharError(pos_start, self.pos, "'" + char + "'")

		tokens.append(Token(TT_EOF, pos_start=self.pos))
		return tokens, None
	
	def skip_comment(self):
		self.advance()

		while self.current_char != '\n' and self.current_char is not None:
			self.advance()

		self.advance()

	def make_number(self):
		num_str = ''
		dot_count = 0
		pos_start = self.pos.copy()

		while self.current_char is not None and self.current_char in DIGITS + '.':
			if self.current_char == '.':
				if dot_count == 1: break
				dot_count += 1
				num_str += '.'
			else:
				num_str += self.current_char
			self.advance()

		if dot_count == 0:
			return Token(TT_INT, int(num_str), pos_start, self.pos)
		else:
			return Token(TT_FLOAT, float(num_str), pos_start, self.pos)

	def make_identifier(self):
		id_str = ''
		pos_start = self.pos.copy()

		while self.current_char is not None and self.current_char in LETTERS_DIGITS + '_':
			id_str += self.current_char
			self.advance()

		tok_type = TT_KEYWORD if id_str in KEYWORDS else TT_IDENTIFIER
		return Token(tok_type, id_str, pos_start, self.pos)
	
	def make_string(self):
		string = ''
		pos_start = self.pos.copy()
		escape_character = False
		self.advance()

		escape_characters = {
			'n': '\n',
			't': '\t'
		}

		while self.current_char is not None and (self.current_char != '"' or escape_character):
			if escape_character:
				string += escape_characters.get(self.current_char, self.current_char)
				escape_character = False
				self.advance()
			else:
				if self.current_char == '\\':
					escape_character = True
					self.advance()
				else:
					string += self.current_char
					self.advance()

		if self.current_char != '"':
			return None, IllegalCharError(pos_start, self.pos, "Unterminated string")

		self.advance()
		return Token(TT_STRING, string, pos_start, self.pos), None

	def make_not_equals(self):
		pos_start = self.pos.copy()
		self.advance()

		if self.current_char == '=':
			self.advance()
			return Token(TT_NE, pos_start=pos_start, pos_end=self.pos), None

		self.advance()
		return None, ExpectedCharError(pos_start, self.pos, "'=' (after '!')")
	
	def make_equals(self):
		tok_type = TT_EQ
		pos_start = self.pos.copy()
		self.advance()

		if self.current_char == '=':
			self.advance()
			tok_type = TT_EE
		elif self.current_char == ">":
			self.advance()
			tok_type = TT_ARROW

		return Token(tok_type, pos_start=pos_start, pos_end=self.pos)

	def make_less_than(self):
		tok_type = TT_LT
		pos_start = self.pos.copy()
		self.advance()

		if self.current_char == '=':
			self.advance()
			tok_type = TT_LTE

		return Token(tok_type, pos_start=pos_start, pos_end=self.pos)

	def make_greater_than(self):
		tok_type = TT_GT
		pos_start = self.pos.copy()
		self.advance()

		if self.current_char == '=':
			self.advance()
			tok_type = TT_GTE

		return Token(tok_type, pos_start=pos_start, pos_end=self.pos)

class NumberNode:
	def __init__(self, tok):
		self.tok = tok
		self.pos_start = self.tok.pos_start
		self.pos_end = self.tok.pos_end

	def __repr__(self):
		return f'{self.tok}'

class StringNode:
	def __init__(self, tok):
		self.tok = tok
		self.pos_start = self.tok.pos_start
		self.pos_end = self.tok.pos_end

	def __repr__(self):
		return f'{self.tok}'

class DictNode:
	def __init__(self, key_value_pairs, pos_start, pos_end):
		self.key_value_pairs = key_value_pairs
		self.pos_start = pos_start
		self.pos_end = pos_end

class ListNode:
	def __init__(self, element_nodes, pos_start, pos_end):
		self.element_nodes = element_nodes
		self.pos_start = pos_start
		self.pos_end = pos_end

class ImportNode:
	def __init__(self, func_name_tok, file_name_tok, pos_start, pos_end):
		self.func_name_tok = func_name_tok
		self.file_name_tok = file_name_tok
		self.pos_start = pos_start
		self.pos_end = pos_end

class RaiseNode:
	def __init__(self, error, value, pos_start, pos_end):
		self.error = error
		self.value = value
		self.pos_start = pos_start
		self.pos_end = pos_end

class VarAccessNode:
	def __init__(self, var_name_tok):
		self.var_name_tok = var_name_tok
		self.pos_start = self.var_name_tok.pos_start
		self.pos_end = self.var_name_tok.pos_end

class VarAssignNode:
	def __init__(self, var_name_tok, value_node, var_name2_tok=None, var_name3_tok=None):
		self.var_name_tok = var_name_tok
		self.var_name2_tok = var_name2_tok
		self.var_name3_tok = var_name3_tok
		self.value_node = value_node
		
		if isinstance(var_name_tok, list):
			self.pos_start = self.var_name_tok[0].pos_start
		else:
			self.pos_start = self.var_name_tok.pos_start
			
		self.pos_end = self.var_name3_tok.pos_end if var_name3_tok else var_name_tok.pos_end

class BinOpNode:
	def __init__(self, left_node, op_tok, right_node):
		self.left_node = left_node
		self.op_tok = op_tok
		self.right_node = right_node
		self.pos_start = self.left_node.pos_start
		self.pos_end = self.right_node.pos_end

	def __repr__(self):
		return f'({self.left_node}, {self.op_tok}, {self.right_node})'

class UnaryOpNode:
	def __init__(self, op_tok, node):
		self.op_tok = op_tok
		self.node = node
		self.pos_start = self.op_tok.pos_start
		self.pos_end = node.pos_end

	def __repr__(self):
		return f'({self.op_tok}, {self.node})'

class IfNode:
	def __init__(self, cases, else_case):
		self.cases = cases
		self.else_case = else_case
		self.pos_start = self.cases[0][0].pos_start
		self.pos_end = (self.else_case or self.cases[len(self.cases) - 1][0]).pos_end

class CheckNode:
	def __init__(self, condition, pos_start, pos_end, error=None):
		self.error = error
		self.condition = condition
		self.pos_start = pos_start
		self.pos_end = pos_end

class ForNode:
	def __init__(self, var_name_tok, start_value_node, end_value_node, step_value_node, body_node, should_return_null):
		self.var_name_tok = var_name_tok
		self.start_value_node = start_value_node
		self.end_value_node = end_value_node
		self.step_value_node = step_value_node
		self.body_node = body_node
		self.should_return_null = should_return_null
		self.pos_start = self.var_name_tok.pos_start
		self.pos_end = self.body_node.pos_end

class WhileNode:
	def __init__(self, condition_node, body_node, should_return_null):
		self.condition_node = condition_node
		self.body_node = body_node
		self.should_return_null = should_return_null
		self.pos_start = self.condition_node.pos_start
		self.pos_end = self.body_node.pos_end

class FuncDefNode:
	def __init__(self, var_name_tok, arg_name_toks, body_node, should_auto_return):
		self.var_name_tok = var_name_tok
		self.arg_name_toks = arg_name_toks
		self.body_node = body_node
		self.should_auto_return = should_auto_return

		if self.var_name_tok:
			self.pos_start = self.var_name_tok.pos_start
		elif len(self.arg_name_toks) > 0:
			self.pos_start = self.arg_name_toks[0].pos_start
		else:
			self.pos_start = self.body_node.pos_start

		self.pos_end = self.body_node.pos_end

class CallNode:
	def __init__(self, node_to_call, arg_nodes):
		self.node_to_call = node_to_call
		self.arg_nodes = arg_nodes
		self.pos_start = self.node_to_call.pos_start

		if len(self.arg_nodes) > 0:
			self.pos_end = self.arg_nodes[len(self.arg_nodes) - 1].pos_end
		else:
			self.pos_end = self.node_to_call.pos_end

class ReturnNode:
	def __init__(self, nodes_to_return, pos_start, pos_end):
		self.nodes_to_return = nodes_to_return
		self.pos_start = pos_start
		self.pos_end = pos_end

class ContinueNode:
	def __init__(self, pos_start, pos_end):
		self.pos_start = pos_start
		self.pos_end = pos_end

class BreakNode:
	def __init__(self, pos_start, pos_end):
		self.pos_start = pos_start
		self.pos_end = pos_end

class ParseResult:
	def __init__(self):
		self.error = None
		self.node = None
		self.advance_count = 0
		self.last_registered_advance_count = 0
		self.to_reverse_count = 0

	def register_advancement(self):
		self.last_registered_advance_count = 1
		self.advance_count += 1

	def register(self, res):
		self.last_registered_advance_count = res.advance_count
		self.advance_count += res.advance_count
		if res.error: self.error = res.error
		return res.node

	def success(self, node):
		self.node = node
		return self

	def failure(self, error):
		if not self.error or self.last_registered_advance_count == 0:
			self.error = error
		return self
	
	def try_register(self, res):
		if res.error:
			self.to_reverse_count = res.advance_count
			return None
		return self.register(res)

class Parser:
	def __init__(self, tokens):
		self.tokens = tokens
		self.tok_idx = -1
		self.advance()

	def advance(self):
		self.tok_idx += 1
		self.update_current_tok()
		return self.current_tok
	
	def reverse(self, amount=1):
		self.tok_idx -= amount
		self.update_current_tok()
		return self.current_tok
	
	def update_current_tok(self):
		if 0 <= self.tok_idx < len(self.tokens):
			self.current_tok = self.tokens[self.tok_idx]

	def parse(self):
		res = self.statements()
		if not res.error and self.current_tok.type != TT_EOF:
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"Expected '+', '-', '*', '/', '^', '==', '!=', '<', '>', '<=', '>=', 'and' or 'or'"
			))
		return res

	def statements(self):
		res = ParseResult()
		statements = []
		pos_start = self.current_tok.pos_start.copy()

		while self.current_tok.type == TT_NEWLINE:
			res.register_advancement()
			self.advance()

		statement = res.register(self.statement())
		if res.error: return res
		statements.append(statement)

		more_statements = True

		while True:
			newline_count = 0
			while self.current_tok.type == TT_NEWLINE:
				res.register_advancement()
				self.advance()
				newline_count += 1
			if newline_count == 0:
				more_statements = False

			if not more_statements: break
			statement = res.try_register(self.statement())
			if not statement:
				self.reverse(res.to_reverse_count)
				more_statements = False
				continue
			statements.append(statement)

		return res.success(ListNode(
			statements,
			pos_start,
			self.current_tok.pos_end.copy()
		))

	def statement(self):
		res = ParseResult()
		pos_start = self.current_tok.pos_start.copy()

		if self.current_tok.matches(TT_KEYWORD, 'return'):
			res.register_advancement()
			self.advance()
			
			# Handle multiple returns
			return_nodes = []
			if self.current_tok.type not in (TT_NEWLINE, TT_EOF):
				first_expr = res.register(self.expr())
				if res.error: return res
				return_nodes.append(first_expr)

				while self.current_tok.type == TT_COMMA:
					res.register_advancement()
					self.advance()

					expr = res.register(self.expr())
					if res.error: return res
					return_nodes.append(expr)

			return res.success(ReturnNode(
				return_nodes if return_nodes else None,
				pos_start,
				self.current_tok.pos_start.copy()
			))

		if self.current_tok.matches(TT_KEYWORD, 'continue'):
			res.register_advancement()
			self.advance()
			return res.success(ContinueNode(pos_start, self.current_tok.pos_start.copy()))

		if self.current_tok.matches(TT_KEYWORD, 'break'):
			res.register_advancement()
			self.advance()
			return res.success(BreakNode(pos_start, self.current_tok.pos_start.copy()))

		if self.current_tok.matches(TT_KEYWORD, 'raise'):
			res.register_advancement()
			self.advance()
			return res.success(self.raise_expr())

		expr = res.register(self.expr())
		if res.error: return res.failure(InvalidSyntaxError(
			self.current_tok.pos_start, self.current_tok.pos_end,
			"Expected expression"
		))
		return res.success(expr)

	def import_expr(self):
		res = ParseResult()
		
		if not self.current_tok.matches(TT_KEYWORD, 'import'):
			return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected 'import'"
            ))

		pos_start = self.current_tok.pos_start.copy()
		self.advance()
		res.register_advancement()
		module_name = self.current_tok.value

		if self.current_tok.type != TT_IDENTIFIER:
			return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected identifier after 'import'"
            ))
		
		func_name_tok = []
		func_name_tok.append(self.current_tok)
		res.register_advancement()
		self.advance()

		while self.current_tok.type == TT_COMMA:
			res.register_advancement(); self.advance()
			if self.current_tok.type != TT_IDENTIFIER:
				return res.failure(InvalidSyntaxError(self.current_tok.pos_start.copy(), self.current_tok.pos_end.copy(),
                "Expected identifier after ','"
                ))
			func_name_tok.append(self.current_tok)
			res.register_advancement(); self.advance()


		if not self.current_tok.matches(TT_KEYWORD, 'from'):
			return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected 'from'"
            ))

		res.register_advancement()
		self.advance()

		if self.current_tok.type != TT_STRING:
			return res.failure(InvalidSyntaxError(self.current_tok.pos_start.copy(), self.current_tok.pos_end.copy(),
			"Expected file name as String"
			))
		
		file_name_tok = self.current_tok
		res.register_advancement()
		self.advance()

		return res.success(ImportNode(func_name_tok, file_name_tok, pos_start, self.current_tok.pos_end.copy()))

	def if_expr(self):
		res = ParseResult()
		all_cases = res.register(self.if_expr_cases('if'))
		if res.error: return res
		cases, else_case = all_cases
		return res.success(IfNode(cases, else_case))

	def raise_expr(self):
		res = ParseResult()
		pos_start = self.current_tok.pos_start.copy()
		if not self.current_tok.matches(TT_KEYWORD, 'raise'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"Expected 'raise'"
			))

		res.register_advancement()
		self.advance()

		if self.current_tok.value == "InvalidSyntaxError":
			to_raise = "InvalidSyntaxError"
		elif self.current_tok.value == "InvalidOperationError":
			to_raise = "InvalidOperationError"
		elif self.current_tok.value == "CheckConditionError":
			to_raise = "CheckConditionError"
		elif self.current_tok.value == "ExpectedCharError":
			to_raise = "ExpectedCharError"
		elif self.current_tok.value == "RuntimeError":
			to_raise = "RuntimeError"
		else:
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Invalid raise type '{self.current_tok.value}'"
			))

		res.register_advancement()
		self.advance()

		if not self.current_tok.matches(TT_KEYWORD, 'as'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"Expected 'as'"
			))
		res.register_advancement()
		self.advance()

		if not self.current_tok.type == TT_STRING:
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"Expected string literal"
			))

		value = self.current_tok.value
		res.register_advancement()
		self.advance()
		return res.success(RaiseNode(to_raise, value, pos_start, self.current_tok.pos_end.copy()))

	def check_expr(self):
		res = ParseResult()
		pos_start = self.current_tok.pos_start.copy()
		
		if not self.current_tok.matches(TT_KEYWORD, 'check'):
			return res.failure(InvalidSyntaxError(
				pos_start, self.current_tok.pos_end,
				"Expected 'check'"
			))
		
		res.register_advancement()
		self.advance()
		
		condition = res.register(self.expr())
		if res.error: return res
		
		if self.current_tok.matches(TT_KEYWORD, 'as'):
			res.register_advancement()
			self.advance()
			
			if self.current_tok.type != TT_STRING:
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Expected string after 'as'"
				))
			
			error_msg = self.current_tok.value
			res.register_advancement()
			self.advance()
			
			return res.success(CheckNode(
				condition,
				pos_start,
				self.current_tok.pos_end.copy(),
				error_msg
			))
		else:
			return res.success(CheckNode(
				condition,
				pos_start,
				self.current_tok.pos_end.copy()
			))

	def if_expr_b(self):
		return self.if_expr_cases('elif')
	
	def if_expr_c(self):
		res = ParseResult()
		else_case = None

		if self.current_tok.matches(TT_KEYWORD, 'else'):
			res.register_advancement()
			self.advance()

			if self.current_tok.type == TT_NEWLINE:
				res.register_advancement()
				self.advance()

				statements = res.register(self.statements())
				if res.error: return res
				else_case = (statements, True)

				if self.current_tok.matches(TT_KEYWORD, 'end'):
					res.register_advancement()
					self.advance()
				else:
					return res.failure(InvalidSyntaxError(
						self.current_tok.pos_start, self.current_tok.pos_end,
						"Expected 'end'"
					))
			else:
				expr = res.register(self.expr())
				if res.error: return res
				else_case = (expr, False)

		return res.success(else_case)

	def if_expr_b_or_c(self):
		res = ParseResult()
		cases, else_case = [], None

		if self.current_tok.matches(TT_KEYWORD, 'elif'):
			all_cases = res.register(self.if_expr_b())
			if res.error: return res
			cases, else_case = all_cases
		else:
			else_case = res.register(self.if_expr_c())
			if res.error: return res
		
		return res.success((cases, else_case))

	def if_expr_cases(self, case_keyword):
		res = ParseResult()
		cases = []
		else_case = None

		if not self.current_tok.matches(TT_KEYWORD, case_keyword):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected '{case_keyword}'"
			))

		res.register_advancement()
		self.advance()

		condition = res.register(self.expr())
		if res.error: return res

		if not self.current_tok.matches(TT_KEYWORD, 'do'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected 'do'"
			))

		res.register_advancement()
		self.advance()

		if self.current_tok.type == TT_NEWLINE:
			res.register_advancement()
			self.advance()

			statements = res.register(self.statements())
			if res.error: return res
			cases.append((condition, statements, True))

			if self.current_tok.matches(TT_KEYWORD, 'end'):
				res.register_advancement()
				self.advance()
			else:
				all_cases = res.register(self.if_expr_b_or_c())
				if res.error: return res
				new_cases, else_case = all_cases
				cases.extend(new_cases)
		else:
			expr = res.register(self.statement())
			if res.error: return res
			cases.append((condition, expr, False))

			all_cases = res.register(self.if_expr_b_or_c())
			if res.error: return res
			new_cases, else_case = all_cases
			cases.extend(new_cases)

		return res.success((cases, else_case))

	def for_expr(self):
		res = ParseResult()

		if not self.current_tok.matches(TT_KEYWORD, 'range'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected 'range'"
			))

		res.register_advancement()
		self.advance()

		if self.current_tok.type != TT_IDENTIFIER:
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected identifier"
			))

		var_name = self.current_tok
		res.register_advancement()
		self.advance()

		if self.current_tok.type != TT_EQ:
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected '='"
			))
		
		res.register_advancement()
		self.advance()

		start_value = res.register(self.expr())
		if res.error: return res

		if not self.current_tok.type == TT_COMMA:
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected ','"
			))

		res.register_advancement()
		self.advance()

		if not self.current_tok.matches(TT_KEYWORD, 'to'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected 'to'"
			))
		
		res.register_advancement()
		self.advance()

		end_value = res.register(self.expr())
		if res.error: return res

		if self.current_tok.type == TT_COMMA:
			res.register_advancement()
			self.advance()
			
			if not self.current_tok.matches(TT_KEYWORD, 'change'):
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Expected 'change' after comma"
				))
			
			res.register_advancement()
			self.advance()

			step_value = res.register(self.expr())
			if res.error: return res
		else:
			step_value = None

		if not self.current_tok.matches(TT_KEYWORD, 'do'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected 'do'"
			))

		res.register_advancement()
		self.advance()

		if self.current_tok.type == TT_NEWLINE:
			res.register_advancement()
			self.advance()

			body = res.register(self.statements())
			if res.error: return res

			if not self.current_tok.matches(TT_KEYWORD, 'end'):
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					f"Expected 'end'"
				))

			res.register_advancement()
			self.advance()

			return res.success(ForNode(var_name, start_value, end_value, step_value, body, True))

		body = res.register(self.statement())
		if res.error: return res

		return res.success(ForNode(var_name, start_value, end_value, step_value, body, False))

	def while_expr(self):
		res = ParseResult()

		if not self.current_tok.matches(TT_KEYWORD, 'while'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected 'while' keyword"
			))

		res.register_advancement()
		self.advance()

		condition = res.register(self.expr())
		if res.error: return res

		if not self.current_tok.matches(TT_KEYWORD, 'do'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected 'do'"
			))

		res.register_advancement()
		self.advance()

		if self.current_tok.type == TT_NEWLINE:
			res.register_advancement()
			self.advance()

			body = res.register(self.statements())
			if res.error: return res

			if not self.current_tok.matches(TT_KEYWORD, 'end'):
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					f"Expected 'end'"
				))

			res.register_advancement()
			self.advance()

			return res.success(WhileNode(condition, body, True))

		body = res.register(self.statement())
		if res.error: return res

		return res.success(WhileNode(condition, body, False))

	def call(self):
		res = ParseResult()
		atom = res.register(self.atom())
		if res.error: return res

		if self.current_tok.type == TT_LPAREN:
			res.register_advancement()
			self.advance()
			arg_nodes = []

			if self.current_tok.type == TT_RPAREN:
				res.register_advancement()
				self.advance()
			else:
				arg_nodes.append(res.register(self.expr()))
				if res.error:
					return res.failure(InvalidSyntaxError(
						self.current_tok.pos_start, self.current_tok.pos_end,
						"Expected ')', 'nexus', 'if', 'range', 'while', 'def', int, float, identifier, '+', '-', '(', '[' or 'not'"
					))

				while self.current_tok.type == TT_COMMA:
					res.register_advancement()
					self.advance()

					arg_nodes.append(res.register(self.expr()))
					if res.error: return res

				if self.current_tok.type != TT_RPAREN:
					return res.failure(InvalidSyntaxError(
						self.current_tok.pos_start, self.current_tok.pos_end,
						f"Expected ',' or ')'"
					))

				res.register_advancement()
				self.advance()
			return res.success(CallNode(atom, arg_nodes))
		return res.success(atom)

	def atom(self):
		res = ParseResult()
		tok = self.current_tok

		if tok.type in (TT_INT, TT_FLOAT):
			res.register_advancement()
			self.advance()
			return res.success(NumberNode(tok))

		elif tok.type == TT_STRING:
			res.register_advancement()
			self.advance()
			return res.success(StringNode(tok))

		elif tok.type == TT_IDENTIFIER:
			res.register_advancement()
			self.advance()
			return res.success(VarAccessNode(tok))

		elif tok.type == TT_LPAREN:
			res.register_advancement()
			self.advance()
			expr = res.register(self.expr())
			if res.error: return res
			if self.current_tok.type == TT_RPAREN:
				res.register_advancement()
				self.advance()
				return res.success(expr)
			else:
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Expected ')'"
				))
		
		elif tok.type == TT_LBRACE:
			dict_expr = res.register(self.dict_expr())
			if res.error: return res
			return res.success(dict_expr)
		
		elif tok.type == TT_LSQUARE:
			list_expr = res.register(self.list_expr())
			if res.error: return res
			return res.success(list_expr)

		if tok.matches(TT_KEYWORD, 'import'):
			import_expr = res.register(self.import_expr())
			if res.error: return res
			return res.success(import_expr)

		elif tok.matches(TT_KEYWORD, 'if'):
			if_expr = res.register(self.if_expr())
			if res.error: return res
			return res.success(if_expr)
		
		elif tok.matches(TT_KEYWORD, 'check'):
			check_expr = res.register(self.check_expr())
			if res.error: return res
			return res.success(check_expr)

		elif tok.matches(TT_KEYWORD, 'range'):
			for_expr = res.register(self.for_expr())
			if res.error: return res
			return res.success(for_expr)

		elif tok.matches(TT_KEYWORD, 'while'):
			while_expr = res.register(self.while_expr())
			if res.error: return res
			return res.success(while_expr)
		
		elif tok.matches(TT_KEYWORD, 'def'):
			func_def = res.register(self.func_def())
			if res.error: return res
			return res.success(func_def)

		return res.failure(InvalidSyntaxError(
			tok.pos_start, tok.pos_end,
			"Expected int, float, identifier, '+', '-', '(', 'range', 'def', 'nexus', 'while', 'if' or '['"
		))
	
	def power(self):
		return self.bin_op(self.call, (TT_POW, ), self.factor)

	def factor(self):
		res = ParseResult()
		tok = self.current_tok

		if tok.type in (TT_PLUS, TT_MINUS):
			res.register_advancement()
			self.advance()
			factor = res.register(self.factor())
			if res.error: return res
			return res.success(UnaryOpNode(tok, factor))

		return self.power()

	def term(self):
		return self.bin_op(self.factor, (TT_MUL, TT_DIV))

	def arith_expr(self):
		return self.bin_op(self.term, (TT_PLUS, TT_MINUS))

	def dict_expr(self):
		res = ParseResult()
		key_value_pairs = []
		pos_start = self.current_tok.pos_start.copy()
		
		if self.current_tok.type != TT_LBRACE:
			return res.failure(InvalidSyntaxError(
                pos_start, self.current_tok.pos_end,
                f"Expected '{TT_LBRACE}'"
            ))
		
		res.register_advancement()
		self.advance()

		if self.current_tok.type == TT_RBRACE:
			res.register_advancement(); self.advance()
			return res.success(DictNode([], pos_start, self.current_tok.pos_end.copy()))

		while True:
			if self.current_tok.type not in (TT_STRING, TT_IDENTIFIER):
				return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    f"Expected string or identifier"
                ))

			key = self.current_tok
			res.register_advancement()
			self.advance()

			if self.current_tok.type != TT_COLON:
				return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    f"Expected ':' after dict key"
                ))

			res.register_advancement()
			self.advance()

			value = res.register(self.expr())
			if res.error: return res

			key_value_pairs.append((key, value))


			if self.current_tok.type == TT_COMMA:
				res.register_advancement(); self.advance()
				continue
			elif self.current_tok.type != TT_RBRACE:
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
                    "Expected ',' or '}'"
				))
			else:
				break
			
		if self.current_tok.type != TT_RBRACE:
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected '}'"
			))
		
		res.register_advancement()
		self.advance()

		return res.success(DictNode(key_value_pairs, pos_start, self.current_tok.pos_end.copy()))
	def list_expr(self):
		res = ParseResult()
		element_nodes = []
		pos_start = self.current_tok.pos_start.copy()

		if self.current_tok.type != TT_LSQUARE:
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected '['"
			))

		res.register_advancement()
		self.advance()

		if self.current_tok.type == TT_RSQUARE:
			res.register_advancement()
			self.advance()
		else:
			element_nodes.append(res.register(self.expr()))
			if res.error:
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Expected ']', 'VAR', 'IF', 'FOR', 'WHILE', 'FUN', int, float, identifier, '+', '-', '(', '[' or 'NOT'"
				))

			while self.current_tok.type == TT_COMMA:
				res.register_advancement()
				self.advance()

				element_nodes.append(res.register(self.expr()))
				if res.error: return res

			if self.current_tok.type != TT_RSQUARE:
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					f"Expected ',' or ']'"
				))

		res.register_advancement()
		self.advance()

		return res.success(ListNode(
			element_nodes,
			pos_start,
			self.current_tok.pos_end.copy()
		))

	def comp_expr(self):
		res = ParseResult()

		if self.current_tok.matches(TT_KEYWORD, 'not'):
			op_tok = self.current_tok
			res.register_advancement()
			self.advance()

			node = res.register(self.comp_expr())
			if res.error: return res
			return res.success(UnaryOpNode(op_tok, node))
		
		node = res.register(self.bin_op(self.arith_expr, (TT_EE, TT_NE, TT_LT, TT_GT, TT_LTE, TT_GTE)))
		
		if res.error:
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"Expected int, float, identifier, '+', '-', '(', '[' or 'not'"
			))

		return res.success(node)

	def expr(self):
		res = ParseResult()

		if self.current_tok.matches(TT_KEYWORD, 'nexus'):
			v1 = False
			v2 = False
			res.register_advancement()
			self.advance()

			if self.current_tok.type != TT_IDENTIFIER:
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Expected identifier"
				))

			var_name = self.current_tok
			res.register_advancement()
			self.advance()

			if self.current_tok.type == TT_NEWLINE:
				res.register_advancement()
				self.advance()
				return res.success(VarAssignNode(var_name, None))

			elif self.current_tok.type == TT_COMMA:
				res.register_advancement()
				self.advance()
				if self.current_tok.type != TT_IDENTIFIER:
					return res.failure(InvalidSyntaxError(
						self.current_tok.pos_start, self.current_tok.pos_end,
						"Expected identifier after comma"
					))
				var_name2 = self.current_tok
				v1 = True
				res.register_advancement()
				self.advance()

				if self.current_tok.type == TT_COMMA:
					res.register_advancement()
					self.advance()
					if self.current_tok.type != TT_IDENTIFIER:
						return res.failure(InvalidSyntaxError(
							self.current_tok.pos_start, self.current_tok.pos_end,
							"Expected identifier after second comma"
						))
					var_name3 = self.current_tok
					v2 = True
					res.register_advancement()
					self.advance()

			if self.current_tok.type != TT_EQ:
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Expected '='"
				))

			res.register_advancement()
			self.advance()
			expr = res.register(self.expr())
			if res.error: return res
			
			if v1:
				if v2: return res.success(VarAssignNode(var_name, expr, var_name2, var_name3))
				return res.success(VarAssignNode(var_name, expr, var_name2))
			return res.success(VarAssignNode(var_name, expr))

		node = res.register(self.bin_op(self.comp_expr, ((TT_KEYWORD, 'and'), (TT_KEYWORD, 'or'))))

		if res.error:
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"Expected 'nexus', int, float, list, string, identifier, '+', '-', '(', '[' or 'not'"
			))

		return res.success(node)
	
	def func_def(self):
		res = ParseResult()
		
		if not self.current_tok.matches(TT_KEYWORD, 'def'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected 'def'"
			))

		res.register_advancement()
		self.advance()

		if self.current_tok.type != TT_IDENTIFIER:
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected function name"
			))

		func_name_tok = self.current_tok
		res.register_advancement()
		self.advance()

		if self.current_tok.matches(TT_KEYWORD, 'using'):
			res.register_advancement()
			self.advance()
			
			if self.current_tok.type != TT_LPAREN:
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					f"Expected '(' after 'using'"
				))
			
			res.register_advancement()
			self.advance()
			
			arg_name_toks = []
			
			if self.current_tok.type == TT_IDENTIFIER:
				arg_name_toks.append(self.current_tok)
				res.register_advancement()
				self.advance()
				
				while self.current_tok.type == TT_COMMA:
					res.register_advancement()
					self.advance()
					
					if self.current_tok.type != TT_IDENTIFIER:
						return res.failure(InvalidSyntaxError(
							self.current_tok.pos_start, self.current_tok.pos_end,
							f"Expected identifier after comma"
						))
					
					arg_name_toks.append(self.current_tok)
					res.register_advancement()
					self.advance()
				
				if self.current_tok.type != TT_RPAREN:
					return res.failure(InvalidSyntaxError(
						self.current_tok.pos_start, self.current_tok.pos_end,
						f"Expected ')' after arguments"
					))
				
				res.register_advancement()
				self.advance()
		else:
			arg_name_toks = []

		if self.current_tok.type == TT_NEWLINE:
			res.register_advancement()
			self.advance()
			
			body = res.register(self.statements())
			if res.error: return res
			
			if not self.current_tok.matches(TT_KEYWORD, 'end'):
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					f"Expected 'end'"
				))
			
			res.register_advancement()
			self.advance()
			
			return res.success(FuncDefNode(
				func_name_tok,
				arg_name_toks,
				body,
				False
			))
		else:
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected newline or '=>'"
			))

	def bin_op(self, func_a, ops, func_b=None):
		if func_b is None:
			func_b = func_a
		
		res = ParseResult()
		left = res.register(func_a())
		if res.error: return res

		while self.current_tok.type in ops or (self.current_tok.type, self.current_tok.value) in ops:
			op_tok = self.current_tok
			res.register_advancement()
			self.advance()
			right = res.register(func_b())
			if res.error: return res
			left = BinOpNode(left, op_tok, right)

		return res.success(left)

class RTResult:
	def __init__(self):
		self.reset()

	def reset(self):
		self.value = None
		self.error = None
		self.func_return_value = None
		self.loop_should_continue = False
		self.loop_should_break = False

	def register(self, res):
		if res.error: self.error = res.error
		self.func_return_value = res.func_return_value
		self.loop_should_continue = res.loop_should_continue
		self.loop_should_break = res.loop_should_break
		return res.value

	def success(self, value):
		self.reset()
		self.value = value
		return self

	def success_return(self, value):
		self.reset()
		self.func_return_value = value
		return self

	def success_continue(self):
		self.reset()
		self.loop_should_continue = True
		return self

	def success_break(self):
		self.reset()
		self.loop_should_break = True
		return self

	def failure(self, error):
		self.reset()
		self.error = error
		return self
	
	def should_return(self):
		return (
			self.error or
			self.func_return_value or
			self.loop_should_continue or
			self.loop_should_break
		)

class Value:
	def __init__(self):
		self.set_pos()
		self.set_context()

	def set_pos(self, pos_start=None, pos_end=None):
		self.pos_start = pos_start
		self.pos_end = pos_end
		return self

	def set_context(self, context=None):
		self.context = context
		return self

	def added_to(self, other):
		return None, self.illegal_operation(other)

	def subbed_by(self, other):
		return None, self.illegal_operation(other)

	def multed_by(self, other):
		return None, self.illegal_operation(other)

	def dived_by(self, other):
		return None, self.illegal_operation(other)

	def powed_by(self, other):
		return None, self.illegal_operation(other)

	def get_comparison_eq(self, other):
		return None, self.illegal_operation(other)

	def get_comparison_ne(self, other):
		return None, self.illegal_operation(other)

	def get_comparison_lt(self, other):
		return None, self.illegal_operation(other)

	def get_comparison_gt(self, other):
		return None, self.illegal_operation(other)

	def get_comparison_lte(self, other):
		return None, self.illegal_operation(other)

	def get_comparison_gte(self, other):
		return None, self.illegal_operation(other)

	def anded_by(self, other):
		return None, self.illegal_operation(other)

	def ored_by(self, other):
		return None, self.illegal_operation(other)

	def notted(self):
		return None, self.illegal_operation()

	def execute(self, args):
		return RTResult().failure(self.illegal_operation())

	def copy(self):
		raise Exception('No copy method defined')

	def is_true(self):
		return False

	def illegal_operation(self, other=None):
		if not other: other = self
		return RTError(
			self.pos_start, other.pos_end,
			'Illegal operation',
			self.context
		)

class Number(Value):
	def __init__(self, value):
		super().__init__()
		self.value = value

	def added_to(self, other):
		if isinstance(other, Number):
			return Number(self.value + other.value).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

	def subbed_by(self, other):
		if isinstance(other, Number):
			return Number(self.value - other.value).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

	def multed_by(self, other):
		if isinstance(other, Number):
			return Number(self.value * other.value).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

	def dived_by(self, other):
		if isinstance(other, Number):
			if other.value == 0:
				return None, RTError(
					other.pos_start, other.pos_end,
					'Division by zero',
					self.context
				)

			return Number(self.value / other.value).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

	def powed_by(self, other):
		if isinstance(other, Number):
			return Number(self.value ** other.value).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

	def get_comparison_eq(self, other):
		if isinstance(other, Number):
			return Number(int(self.value == other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

	def get_comparison_ne(self, other):
		if isinstance(other, Number):
			return Number(int(self.value != other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

	def get_comparison_lt(self, other):
		if isinstance(other, Number):
			return Number(int(self.value < other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

	def get_comparison_gt(self, other):
		if isinstance(other, Number):
			return Number(int(self.value > other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

	def get_comparison_lte(self, other):
		if isinstance(other, Number):
			return Number(int(self.value <= other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

	def get_comparison_gte(self, other):
		if isinstance(other, Number):
			return Number(int(self.value >= other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

	def anded_by(self, other):
		if isinstance(other, Number):
			return Number(int(self.value and other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

	def ored_by(self, other):
		if isinstance(other, Number):
			return Number(int(self.value or other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

	def notted(self):
		return Number(1 if self.value == 0 else 0).set_context(self.context), None

	def copy(self):
		copy = Number(self.value)
		copy.set_pos(self.pos_start, self.pos_end)
		copy.set_context(self.context)
		return copy

	def is_true(self):
		return self.value != 0

	def __repr__(self):
		return str(self.value)

Number.null = Number(0)
Number.false = Number(0)
Number.true = Number(1)

class String(Value):
	def __init__(self, value):
		super().__init__()
		self.value = value
	
	def added_to(self, other):
		if isinstance(other, String):
			return String(self.value + other.value).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)
		
	def multed_by(self, other):
		if isinstance(other, Number):
			return String(self.value * other.value).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

	def get_comparison_eq(self, other):
		if isinstance(other, String):
			return Number(int(self.value == other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

	def get_comparison_ne(self, other):
		if isinstance(other, String):
			return Number(int(self.value != other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)
	
	def is_true(self):
		return len(self.value) > 0 

	def copy(self):
		copy = String(self.value)
		copy.set_pos(self.pos_start, self.pos_end)
		copy.set_context(self.context)
		return copy
	
	def __repr__(self):
		return f'"{self.value}"'
	
	def __str__(self):
		return self.value

class List(Value):
	def __init__(self, elements):
		super().__init__()
		self.elements = elements

	def added_to(self, other):
		new_list = self.copy()
		new_list.elements.append(other)
		return new_list, None
	
	def multed_by(self, other):
		if isinstance(other, List):
			new_list = self.copy()
			new_list.elements.extend(other.elements)
			return new_list, None
		else:
			return None, Value.illegal_operation(self, other)
	
	def subbed_by(self, other):
		if isinstance(other, Number):
			new_list = self.copy()
			try:
				new_list.elements.pop(other.value)
				return new_list, None
			except:
				return None, RTError(
					other.pos_start, other.pos_end,
					"Element was not removed from the list because index is out of bounds",
					self.context
				)
		else:
			return None, Value.illegal_operation(self, other)
	
	def dived_by(self, other):
		if isinstance(other, Number):
			try:
				return self.elements[other.value], None
			except:
				return None, RTError(
					other.pos_start, other.pos_end,
					"Element was not retrieved from the list because index is out of bounds",
					self.context
				)
		else:
			return None, Value.illegal_operation(self, other)
	
	def copy(self):
		copy = List(self.elements)
		copy.set_pos(self.pos_start, self.pos_end)
		copy.set_context(self.context)
		return copy
	
	def __repr__(self):
		return f'[{", ".join([str(x) for x in self.elements])}]'
	
	def __str__(self):
		return ", ".join([str(x) for x in self.elements])

class Dict(Value):
	def __init__(self, items):
		super().__init__()
		self.elements = items
	
	def added_to(self, other):
		if isinstance(other, Dict):
			new_dict = self.copy()
			new_dict.elements.update(other.elements)
			return new_dict, None
		return None, self.illegal_operation(other)
	
	def subbed_by(self, other):
		if isinstance(other, String):
			key = other.value
			if key in self.elements:
				new_dict = self.copy()
				del new_dict.elements[key]
				return new_dict, None
			return None, RTError(
				other.pos_start, other.pos_end,
                f"Key '{key}' not found in dictionary",
                self.context
			)
		return None, self.illegal_operation(other)
	
	def copy(self):
		copy = Dict(self.elements.copy())
		copy.set_pos(self.pos_start, self.pos_end)
		copy.set_context(self.context)
		return copy
	
	def __repr__(self):
		pairs = []
		for key, value in self.elements.items():
			if isinstance(value, String):
				pairs.append(f"'{key}': '{value}'")
			else:
				pairs.append(f"'{key}': {str(value)}")
		return '{' + ', '.join(pairs) + '}'
	
	def __str__(self):
		pairs = []
		for key, value in self.elements.items():
			if isinstance(value, String):
				pairs.append(f"'{key}': '{value}'")
			else:
				pairs.append(f"'{key}': {str(value)}")
		return ', '.join(pairs)

class BaseFunction(Value):
	def __init__(self, name):
		super().__init__()
		self.name = name or '<anonymous>'
	
	def generate_new_context(self):
		new_context = Context(self.name, self.context, self.pos_start)
		new_context.symbol_table = SymbolTable(new_context.parent.symbol_table)
		return new_context

	def check_args(self, arg_names, args):
		res = RTResult()

		if len(args) > len(arg_names):
			return res.failure(RTError(
				self.pos_start, self.pos_end,
				f"{len(args) - len(arg_names)} too many args passed into '{self.name}'",
				self.context
			))
		
		if len(args) < len(arg_names):
			return res.failure(RTError(
				self.pos_start, self.pos_end,
				f"{len(arg_names) - len(args)} too few args passed into '{self.name}'",
				self.context
			))

		return res.success(None)
	
	def populate_args(self, arg_names, args, new_context):
		for i in range(len(args)):
			arg_name = arg_names[i]
			arg_value = args[i]
			arg_value.set_context(new_context)
			new_context.symbol_table.set(arg_name, arg_value)
	
	def check_and_populate_args(self, arg_names, args, new_context):
		res = RTResult()
		res.register(self.check_args(arg_names, args))
		if res.should_return(): return res
		self.populate_args(arg_names, args, new_context)
		return res.success(None)

class Function(BaseFunction):
	def __init__(self, name, body_node, arg_names, should_auto_return):
		super().__init__(name)
		self.body_node = body_node
		self.arg_names = arg_names
		self.should_auto_return = should_auto_return
	
	def execute(self, args):
		res = RTResult()
		interpreter = Interpreter()
		exec_ctx = self.generate_new_context()

		res.register(self.check_and_populate_args(self.arg_names, args, exec_ctx))
		if res.should_return(): return res

		value = res.register(interpreter.visit(self.body_node, exec_ctx))
		if res.should_return() and res.func_return_value is None: return res

		# Return the value directly (could be a List for multiple returns)
		return res.success(res.func_return_value)

	def copy(self):
		copy = Function(self.name, self.body_node, self.arg_names, self.should_auto_return)
		copy.set_context(self.context)
		copy.set_pos(self.pos_start, self.pos_end)
		return copy

	def __repr__(self):
		return f"<function {self.name}>"

class BuiltInFunction(BaseFunction):
	def __init__(self, name):
		super().__init__(name)
	
	def execute(self, args):
		res = RTResult()
		exec_ctx = self.generate_new_context()

		method_name = f'execute_{self.name}'
		method = getattr(self, method_name, self.no_visit_method)

		res.register(self.check_and_populate_args(method.arg_names, args, exec_ctx))
		if res.should_return(): return res

		return_value = res.register(method(exec_ctx))
		if res.should_return(): return res
		return res.success(return_value)

	def no_visit_method(self, node, context):
		raise Exception(f'No execute_{self.name} method defined')
	
	def copy(self):
		copy = BuiltInFunction(self.name)
		copy.set_context(self.context)
		copy.set_pos(self.pos_start, self.pos_end)
		return copy

	def __repr__(self):
		return f"<built-in function {self.name}>"
	
	def execute_write(self, exec_ctx):
		print(str(exec_ctx.symbol_table.get('value')))
		return RTResult().success(Number.null)
	execute_write.arg_names = ["value"]

	def execute_write_ret(self, exec_ctx):
		return RTResult().success(String(str(exec_ctx.symbol_table.get('value'))))
	execute_write_ret.arg_names = ["value"]

	def execute_input(self, exec_ctx):
		text = input(str(exec_ctx.symbol_table.get('prompt')))
		return RTResult().success(String(text))
	execute_input.arg_names = ['prompt']

	def execute_input_int(self, exec_ctx):
		while True:
			text = input(str(exec_ctx.symbol_table.get('prompt')))
			try:
				number = int(text)
				return RTResult().success(Number(number))
			except ValueError:
				print(f"'{text}' must be an integer. Try again!")
	execute_input_int.arg_names = ['prompt']
	
	def execute_clear(self, exec_ctx):
		os.system('cls' if os.name == 'nt' else 'clear')
		return RTResult().success(Number.null)
	execute_clear.arg_names = []

	def execute_is_number(self, exec_ctx):
		is_number = isinstance(exec_ctx.symbol_table.get("value"), Number)
		return RTResult().success(Number.true if is_number else Number.false)
	execute_is_number.arg_names = ["value"]
	
	def execute_is_string(self, exec_ctx):
		is_string = isinstance(exec_ctx.symbol_table.get("value"), String)
		return RTResult().success(Number.true if is_string else Number.false)
	execute_is_string.arg_names = ["value"]

	def execute_is_list(self, exec_ctx):
		is_list = isinstance(exec_ctx.symbol_table.get("value"), List)
		return RTResult().success(Number.true if is_list else Number.false)
	execute_is_list.arg_names = ["value"]

	def execute_is_function(self, exec_ctx):
		is_function = isinstance(exec_ctx.symbol_table.get("value"), BaseFunction)
		return RTResult().success(Number.true if is_function else Number.false)
	execute_is_function.arg_names = ["value"]

	def execute_is_dict(self, exec_ctx):
		is_dict = isinstance(exec_ctx.symbol_table.get("value"), Dict)
		return RTResult().success(Number.true if is_dict else Number.false)
	execute_is_dict.arg_names = ["value"]
	
	def execute_get(self, exec_ctx):
		list_ = exec_ctx.symbol_table.get("list")
		index = exec_ctx.symbol_table.get("index")

		if not isinstance(list_, List) and not isinstance(list_, Dict):
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				"First argument must be a list or dict",
				exec_ctx
			))
		
		if not isinstance(index, Number):
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				"Second argument must be a number",
				exec_ctx
			))
		
		try:
			return RTResult().success(list_.elements[int(index.value)] if isinstance(list_, List) else  list_.elements[index.value])
		except:
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				"Index out of bounds | Key does not exist",
				exec_ctx
			))
	execute_get.arg_names = ["list", "index"]

	def execute_set(self, exec_ctx):
		dict_ = exec_ctx.symbol_table.get("dict")
		key = exec_ctx.symbol_table.get("key")
		value = exec_ctx.symbol_table.get("value")

		if not isinstance(dict_, Dict):
			return RTResult().failure(RTError(
                self.pos_start, self.pos_end,
                "First argument must be a dictionary",
                exec_ctx
            ))
		
		if not isinstance(key, String):
			return RTResult().failure(RTError(
                self.pos_start, self.pos_end,
                "Second argument must be a string",
                exec_ctx
            ))
		
		try:
			dict_.elements[key] = value
			return RTResult().success(dict_)
		except Exception as e:
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
                f"Error setting key '{key}': {str(e)}",
                exec_ctx
			))

	def execute_del(self, exec_ctx):
		list_ = exec_ctx.symbol_table.get("list")
		index = exec_ctx.symbol_table.get("index")

		if not isinstance(list_, List) and not isinstance(list_, Dict):
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				"First argument must be a list or a dict",
				exec_ctx
			))
		
		if not isinstance(index, Number) and not isinstance(index, String):
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				"Second argument must be a number",
				exec_ctx
			))
		
		try:
			if isinstance(list_, List): list_.elements.pop(index.value);
			else: del (list_.elements[index if isinstance(index, String) else None])
			return RTResult().success(list_)
		except:
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				"Index out of bounds",
				exec_ctx
			))
	execute_del.arg_names = ['list', 'index']

	def execute_extend(self, exec_ctx):
		list1 = exec_ctx.symbol_table.get("list1")
		list2 = exec_ctx.symbol_table.get("list2")

		if not isinstance(list1, List):
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				"First argument must be a list",
				exec_ctx
			))
		if not isinstance(list2, List):
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				"Second argument must be a list",
				exec_ctx
			))

		list1.elements.extend(list2.elements)
		return RTResult().success(list1)
	execute_extend.arg_names = ["list1", "list2"]

	def execute_copylist(self, exec_ctx):
		list_ = exec_ctx.symbol_table.get("list")
		if not isinstance(list_, List):
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				"Argument must be a list",
				exec_ctx
			))
		return RTResult().success(List(list_.elements.copy()))
	execute_copylist.arg_names = ['list']
	
	def execute_reverse(self, exec_ctx):
		list_ = exec_ctx.symbol_table.get("list")

		if not isinstance(list_, List):
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				"Argument must be a list",
				exec_ctx
			))

		list_.elements.reverse()
		return RTResult().success(list_)
	execute_reverse.arg_names = ['list']

	def execute_append(self, exec_ctx):
		list_ = exec_ctx.symbol_table.get("list")
		value = exec_ctx.symbol_table.get("value")

		if not isinstance(list_, List):
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				"First argument must be list",
				exec_ctx
			))

		list_.elements.append(value)
		return RTResult().success(list_)
	execute_append.arg_names = ["list", "value"]
	
	def execute_len(self, exec_ctx):
		list_ = exec_ctx.symbol_table.get("list")
		if isinstance(list_, String):
			return RTResult().success(Number(len(list_.value)))
		elif isinstance(list_, List):
			return RTResult().success(Number(len(list_.elements)))
		else:
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				"Argument must be a list or string",
				exec_ctx
			))
	execute_len.arg_names = ['list']

	def execute_run(self, exec_ctx):
		fn = exec_ctx.symbol_table.get("fn")

		if not isinstance(fn, String):
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				"Second argument must be string",
				exec_ctx
			))

		fn = fn.value

		try:
			with open(fn, "r") as f:
				script = f.read()
		except Exception as e:
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				f"Failed to load script \"{fn}\"\n" + str(e),
				exec_ctx
			))

		_, error = run(fn, script)
		if error:
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				f"Failed to finish executing script \"{fn}\"\n" +
				error.as_string(),
				exec_ctx
			))

		return RTResult().success(Number.null)
	execute_run.arg_names = ["fn"]

	def execute_replace(self, exec_ctx):
		list_ = exec_ctx.symbol_table.get("list")
		index = exec_ctx.symbol_table.get("index")
		new_value = exec_ctx.symbol_table.get("new_value")
		if not isinstance(list_, List):
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				"First argument must be a list",
				exec_ctx
			))
		if not isinstance(index, Number) or not isinstance(new_value, (String, Number, List)):
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				"Index must be a integer and new value must be a number, string or list",
				exec_ctx
			))
		try:
			list_.elements[index.value] = new_value
			return RTResult().success(list_)
		except IndexError:
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				"Index out of bounds",
				exec_ctx
			))
		except Exception as e:
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				f"Failed to replace element at index {index.value}\n" + str(e),
				exec_ctx
			))
	execute_replace.arg_names = ["list", "index", "new_value"]

	def execute_str(self, exec_ctx):
		value = exec_ctx.symbol_table.get("value")
		if isinstance(value, Number):
			return RTResult().success(String(str(value.value)))
		elif isinstance(value, String):
			return RTResult().success(String(value.value))
		elif isinstance(value, List):
			return RTResult().success(String("[{}...]".format(", ".join(str(e) for e in value.elements[:10]))))
		elif isinstance(value, Function):
			return RTResult().success(String(f"<function {value.name}>"))
		elif isinstance(value, Dict):
			return RTResult().success(String(f"{len(value.__str__)}"))
		else:
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				"Argument must be a number, string, list, function or dict",
				exec_ctx
			))
	execute_str.arg_names = ['value']

	def execute_list(self, exec_ctx):
		elements = exec_ctx.symbol_table.get("elements")
		return RTResult().success(List(str(elements).split()))

	def execute_int(self, exec_ctx):
		value = exec_ctx.symbol_table.get("value")
		if isinstance(value, Number):
			return RTResult().success(Number(int(value.value)))
		elif isinstance(value, String):
			try:
				return RTResult().success(Number(int(value.value)))
			except ValueError:
				return RTResult().failure(RTError(
					self.pos_start, self.pos_end,
					"Invalid string to convert to integer",
					exec_ctx
				))
		elif isinstance(value, List):
			return RTResult().success(Number(len(value.elements)))
		elif isinstance(value, Function):
			return RTResult().failure(RTError(
					self.pos_start, self.pos_end,
					"Argument must be a number, string, or list.",
					exec_ctx
				))
		else:
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				"Argument must be a number, string, or list.",
				exec_ctx
			))
	execute_int.arg_names = ["value"]

	def execute_type(self, exec_ctx):
		value = exec_ctx.symbol_table.get("value")
		if isinstance(value, Number):
			return RTResult().success(String('int'))
		elif isinstance(value, String):
			return RTResult().success(String('str'))
		elif isinstance(value, List):
			return RTResult().success(String('list'))
		elif isinstance(value, Function):
			return RTResult().success(String('function'))
		elif isinstance(value, Dict):
			return RTResult().success(String('dict'))
		else:
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				"Type check failed",
				exec_ctx
			))
	execute_type.arg_names = ['value']

	def execute_join(self, exec_ctx):
		separator = exec_ctx.symbol_table.get("separator")
		value = exec_ctx.symbol_table.get("value")
		if not isinstance(separator, String) or not isinstance(value, List):
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				"Separator must be a string and value must be a list",
				exec_ctx
			))
		return RTResult().success(String(separator.value.join([str(x) for x in value.elements])))
	execute_join.arg_names = ["separator", "value"]

	def execute_keys(self, exec_ctx):
		value = exec_ctx.symbol_table.get("value")
		if not isinstance(value, Dict):
			return RTResult().failure(RTError(
                self.pos_start, self.pos_end,
                "Argument must be a dict",
                exec_ctx
            ))
		return RTResult().success(List([String(k) for k in value.elements.keys()]))
	execute_keys.arg_names = ["value"]

	def execute_values(self, exec_ctx):
		value = exec_ctx.symbol_table.get("value")
		if not isinstance(value, Dict):
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
                "Argument must be a dict",
                exec_ctx
			))
		return RTResult().success(List([String(v) for v in value.elements.values()]))
	execute_values.arg_names = ["value"]

	def execute_items(self, exec_ctx):
		value = exec_ctx.symbol_table.get("value")
		if not isinstance(value, Dict):
			return RTResult().failure(RTError(
                self.pos_start, self.pos_end,
                "Argument must be a dict",
                exec_ctx
            ))
		return RTResult().success(List([(String(k), v) for k, v in value.elements.items()]))
	execute_items.arg_names = ["value"]

BuiltInFunction.write = BuiltInFunction("write")
BuiltInFunction.write_ret = BuiltInFunction("write_ret")
BuiltInFunction.input = BuiltInFunction("input")
BuiltInFunction.input_int = BuiltInFunction("input_int")
BuiltInFunction.clear = BuiltInFunction("clear")
BuiltInFunction.is_number = BuiltInFunction("is_number")
BuiltInFunction.is_string = BuiltInFunction("is_string")
BuiltInFunction.is_list = BuiltInFunction("is_list")
BuiltInFunction.is_function = BuiltInFunction("is_function")
BuiltInFunction.get = BuiltInFunction("get")
BuiltInFunction.delt = BuiltInFunction("del")
BuiltInFunction.extend = BuiltInFunction("extend")
BuiltInFunction.reverse = BuiltInFunction("reverse")
BuiltInFunction.len = BuiltInFunction("len")
BuiltInFunction.copylist = BuiltInFunction("copylist")
BuiltInFunction.append = BuiltInFunction("append")
BuiltInFunction.run = BuiltInFunction("run")
BuiltInFunction.replace = BuiltInFunction("replace")
BuiltInFunction.str = BuiltInFunction("str")
BuiltInFunction.int = BuiltInFunction("int")
BuiltInFunction.join = BuiltInFunction("join")
BuiltInFunction.type = BuiltInFunction("type")
BuiltInFunction.keys = BuiltInFunction("keys")
BuiltInFunction.values = BuiltInFunction("values")
BuiltInFunction.items = BuiltInFunction("items")

class Context:
	def __init__(self, display_name, parent=None, parent_entry_pos=None):
		self.display_name = display_name
		self.parent = parent
		self.parent_entry_pos = parent_entry_pos
		self.symbol_table = None

class SymbolTable:
	def __init__(self, parent=None):
		self.symbols = {}
		self.parent = parent

	def get(self, name):
		value = self.symbols.get(name, None)
		if value is None and self.parent:
			return self.parent.get(name)
		return value

	def set(self, name, value):
		self.symbols[name] = value

	def remove(self, name):
		del self.symbols[name]

class Interpreter:
	def visit(self, node, context):
		method_name = f'visit_{type(node).__name__}'
		method = getattr(self, method_name, self.no_visit_method)
		return method(node, context)

	def no_visit_method(self, node, context):
		raise Exception(f'No visit_{type(node).__name__} method defined')

	def visit_NumberNode(self, node, context):
		return RTResult().success(
			Number(node.tok.value).set_context(context).set_pos(node.pos_start, node.pos_end))
	
	def visit_StringNode(self, node, context):
		return RTResult().success(
			String(node.tok.value).set_context(context).set_pos(node.pos_start, node.pos_end))
	
	def visit_ImportNode(self, node, context):
		res = RTResult()
		filename = node.file_name_tok.value
		
		if filename == "stdlibs": filename = f"../NexusLang/stdlibs.nxs"
		else:
			if not str(filename).endsWith('.nxs'): filename += '.nxs'
			if not str(filename).startsWith('../'): filename = f"../{filename}"

		try:
			# Read module file
			with open(filename, "r") as f:
				script = f.read()
		except Exception as e:
			return res.failure(RTError(
				node.pos_start, node.pos_end,
				f"Could not load module '{node.file_name_tok.value}': {str(e)}",
				context
			))
		
		# Create isolated module context
		module_context = Context(f"<module {node.file_name_tok.value}>")
		module_context.symbol_table = SymbolTable(global_symbol_table)
		
		# Run module in its isolated context
		_, error, ctx = run(filename, file=filename, context=module_context)
		if error: return res.failure(error)
		module_context = ctx
		ctx.symbol_table.parent = global_symbol_table
		# Import specified functions
		for func_tok in node.func_name_tok:
			func_value = module_context.symbol_table.get(func_tok.value)
			if not func_value:
				return res.failure(RTError(
					node.pos_start, node.pos_end,
					f"Function '{func_tok.value}' not exported by module",
					context
				))
			
			# Create copy with current context
			func_copy = func_value.copy()
			func_copy.set_context(context)
			context.symbol_table.set(func_tok.value, func_copy)
		
		return res.success(Number.null)

	def visit_ListNode(self, node, context):
		res = RTResult()
		elements = []

		for element_node in node.element_nodes:
			elements.append(res.register(self.visit(element_node, context)))
			if res.should_return(): return res
		
		return res.success(
			List(elements).set_context(context).set_pos(node.pos_start, node.pos_end))
	
	def visit_DictNode(self, node, context):
		res = RTResult()
		elements = {}

		for key_tok, value_node in node.key_value_pairs:
			value = res.register(self.visit(value_node, context))
			if res.error: return res

			elements[key_tok.value] = value
		
		return res.success(
            Dict(elements).set_context(context).set_pos(node.pos_start, node.pos_end)
		)

	def visit_VarAccessNode(self, node, context):
		res = RTResult()
		var_name = node.var_name_tok.value
		value = context.symbol_table.get(var_name)

		if value is None:
			return res.failure(RTError(
				node.pos_start, node.pos_end,
				f"'{var_name}' is not defined",
				context
			))

		value = value.copy().set_pos(node.pos_start, node.pos_end).set_context(context)
		return res.success(value)

	def visit_VarAssignNode(self, node, context):
		res = RTResult()
    
		value = res.register(self.visit(node.value_node, context))
		if res.error: return res
		
		# Handle multiple assignment from list return
		if isinstance(value, List) and (node.var_name2_tok or node.var_name3_tok):
			vars_to_assign = [
				node.var_name_tok.value,
				node.var_name2_tok.value if node.var_name2_tok else None,
				node.var_name3_tok.value if node.var_name3_tok else None
			]
			
			for i, var in enumerate(vars_to_assign):
				if var is not None and i < len(value.elements):
					context.symbol_table.set(var, value.elements[i])
			return res.success(value.elements[-1] if value.elements else Number.null)
		else:
			# Regular assignment
			context.symbol_table.set(node.var_name_tok.value, value)
			return res.success(value)

	def visit_BinOpNode(self, node, context):
		res = RTResult()
		left = res.register(self.visit(node.left_node, context))
		if res.should_return(): return res
		right = res.register(self.visit(node.right_node, context))
		if res.should_return(): return res

		if node.op_tok.type == TT_PLUS:
			result, error = left.added_to(right)
		elif node.op_tok.type == TT_MINUS:
			result, error = left.subbed_by(right)
		elif node.op_tok.type == TT_MUL:
			result, error = left.multed_by(right)
		elif node.op_tok.type == TT_DIV:
			result, error = left.dived_by(right)
		elif node.op_tok.type == TT_POW:
			result, error = left.powed_by(right)
		elif node.op_tok.type == TT_EE:
			result, error = left.get_comparison_eq(right)
		elif node.op_tok.type == TT_NE:
			result, error = left.get_comparison_ne(right)
		elif node.op_tok.type == TT_LT:
			result, error = left.get_comparison_lt(right)
		elif node.op_tok.type == TT_GT:
			result, error = left.get_comparison_gt(right)
		elif node.op_tok.type == TT_LTE:
			result, error = left.get_comparison_lte(right)
		elif node.op_tok.type == TT_GTE:
			result, error = left.get_comparison_gte(right)
		elif node.op_tok.matches(TT_KEYWORD, 'and'):
			result, error = left.anded_by(right)
		elif node.op_tok.matches(TT_KEYWORD, 'or'):
			result, error = left.ored_by(right)

		if error:
			return res.failure(error)
		else:
			return res.success(result.set_pos(node.pos_start, node.pos_end))

	def visit_UnaryOpNode(self, node, context):
		res = RTResult()
		number = res.register(self.visit(node.node, context))
		if res.should_return(): return res

		error = None

		if node.op_tok.type == TT_MINUS:
			number, error = number.multed_by(Number(-1))
		elif node.op_tok.matches(TT_KEYWORD, 'not'):
			number, error = number.notted()

		if error:
			return res.failure(error)
		else:
			return res.success(number.set_pos(node.pos_start, node.pos_end))

	def visit_RaiseNode(self, node, context):
		res = RTResult()
		error = node.error
		value = node.value

		if error == "InvalidSyntaxError": 
			return res.failure(InvalidSyntaxError(node.pos_start, node.pos_end, value if value else ""))
		elif error == "ExpectedCharError": 
			return res.failure(ExpectedCharError(node.pos_start, node.pos_end, value if value else ""))
		elif error == "CheckConditionError": 
			return res.failure(CheckConditionError(node.pos_start, node.pos_end, value if value else ""))
		elif error == "RuntimeError": 
			return res.failure(RTError(node.pos_start, node.pos_end, value if value else "", context))
		else: 
			return res.failure(RTError(node.pos_start, node.pos_end, value if value else "", context))

	def visit_IfNode(self, node, context):
		res = RTResult()

		for condition, expr, should_return_null in node.cases:
			condition_value = res.register(self.visit(condition, context))
			if res.should_return(): return res

			if condition_value.is_true():
				expr_value = res.register(self.visit(expr, context))
				if res.should_return(): return res
				return res.success(Number.null if should_return_null else expr_value)

		if node.else_case:
			expr, should_return_null = node.else_case
			expr_value = res.register(self.visit(expr, context))
			if res.should_return(): return res
			return res.success(Number.null if should_return_null else expr_value)

		return res.success(Number.null)

	def visit_CheckNode(self, node, context):
		res = RTResult()
		condition = res.register(self.visit(node.condition, context))
		if res.should_return(): return res

		if condition.is_true():
			return res.success(Number.true)
		else:
			return res.failure(CheckConditionError(
				node.pos_start, node.pos_end,
				node.error if node.error else f"Expected condition to be True, but got {condition.value}"
			))

	def visit_ForNode(self, node, context):
		res = RTResult()
		elements = []

		start_value = res.register(self.visit(node.start_value_node, context))
		if res.should_return(): return res

		end_value = res.register(self.visit(node.end_value_node, context))
		if res.should_return(): return res

		if node.step_value_node:
			step_value = res.register(self.visit(node.step_value_node, context))
			if res.should_return(): return res
		else:
			step_value = Number(1)

		i = start_value.value

		if step_value.value >= 0:
			condition = lambda: i < end_value.value
		else:
			condition = lambda: i > end_value.value

		while condition():
			context.symbol_table.set(node.var_name_tok.value, Number(i))
			i += step_value.value

			value = res.register(self.visit(node.body_node, context))
			if res.should_return() and not res.loop_should_continue and not res.loop_should_break: return res

			if res.loop_should_continue:
				continue
			elif res.loop_should_break:
				break

			elements.append(value)

		return res.success(
			Number.null if node.should_return_null else
			List(elements).set_context(context).set_pos(node.pos_start, node.pos_end))

	def visit_WhileNode(self, node, context):
		res = RTResult()
		elements = []

		while True:
			condition = res.register(self.visit(node.condition_node, context))
			if res.should_return(): return res

			if not condition.is_true(): break

			value = res.register(self.visit(node.body_node, context))
			if res.should_return() and not res.loop_should_continue and not res.loop_should_break: return res

			if res.loop_should_continue:
				continue
			elif res.loop_should_break:
				break

			elements.append(value)

		return res.success(
			Number.null if node.should_return_null else
			List(elements).set_context(context).set_pos(node.pos_start, node.pos_end))

	def visit_FuncDefNode(self, node, context):
		res = RTResult()

		func_name = node.var_name_tok.value if node.var_name_tok else None
		body_node = node.body_node
		arg_names = [arg_name.value for arg_name in node.arg_name_toks]
		func_value = Function(func_name, body_node, arg_names, node.should_auto_return).set_context(context).set_pos(node.pos_start, node.pos_end)

		if node.var_name_tok:
			context.symbol_table.set(func_name, func_value)

		return res.success(func_value)

	def visit_CallNode(self, node, context):
		res = RTResult()
		args = []

		value_to_call = res.register(self.visit(node.node_to_call, context))
		if res.error: return res

		for arg_node in node.arg_nodes:
			args.append(res.register(self.visit(arg_node, context)))
			if res.error: return res

		return_value = res.register(value_to_call.execute(args))
		if res.error: return res

		return res.success(return_value)

	def visit_ReturnNode(self, node, context):
		res = RTResult()
		
		if node.nodes_to_return is None:
			return res.success_return(Number.null)
		
		# Evaluate all return expressions
		values = []
		for expr_node in node.nodes_to_return:
			value = res.register(self.visit(expr_node, context))
			if res.error: return res
			values.append(value)
		
		# Return as a List if multiple values, or single value if just one
		if len(values) > 1:
			return res.success_return(List(values))
		else:
			return res.success_return(values[0] if values else Number.null)

	def visit_ContinueNode(self, node, context):
		return RTResult().success_continue()

	def visit_BreakNode(self, node, context):
		return RTResult().success_break()

global_symbol_table = SymbolTable()
global_symbol_table.set("null", Number.null)
global_symbol_table.set("false", Number.false)
global_symbol_table.set("true", Number.true)
global_symbol_table.set("write", BuiltInFunction.write)
global_symbol_table.set("writert", BuiltInFunction.write_ret)
global_symbol_table.set("input", BuiltInFunction.input)
global_symbol_table.set("int_input", BuiltInFunction.input_int)
global_symbol_table.set("clear", BuiltInFunction.clear)
global_symbol_table.set("cls", BuiltInFunction.clear)
global_symbol_table.set("isint", BuiltInFunction.is_number)
global_symbol_table.set("isstr", BuiltInFunction.is_string)
global_symbol_table.set("islist", BuiltInFunction.is_list)
global_symbol_table.set("isfunc", BuiltInFunction.is_function)
global_symbol_table.set("get", BuiltInFunction.get)
global_symbol_table.set("del", BuiltInFunction.delt)
global_symbol_table.set("extend", BuiltInFunction.extend)
global_symbol_table.set("reverse", BuiltInFunction.reverse)
global_symbol_table.set("len", BuiltInFunction.len)
global_symbol_table.set("copy", BuiltInFunction.copylist)
global_symbol_table.set("append", BuiltInFunction.append)
global_symbol_table.set("run", BuiltInFunction.run)
global_symbol_table.set("int", BuiltInFunction.int)
global_symbol_table.set("str", BuiltInFunction.str)
global_symbol_table.set("join", BuiltInFunction.join)
global_symbol_table.set("type", BuiltInFunction.type)
global_symbol_table.set("keys", BuiltInFunction.keys)
global_symbol_table.set("values", BuiltInFunction.values)
global_symbol_table.set("items", BuiltInFunction.items)
global_symbol_table.set("isdict", BuiltInFunction("is_dict"))

def run(fn, text=None, file=None, context=None):
	# Generate tokens
	file = str(file) if file else None
	if file:
		if not str(file).endswith('.nxs'): file += '.nxs'
		with open(file, 'r') as f:
			script = f.read()
	else:
		script = text
	
	lexer = Lexer(fn, script)
	tokens, error = lexer.make_tokens()
	if error: return None, error
	
	# Generate AST
	parser = Parser(tokens)
	ast = parser.parse()
	if ast.error: return None, ast.error

	# Run program
	interpreter = Interpreter()
	context = Context(file if file else "<program>")
	context.symbol_table = global_symbol_table
	result = interpreter.visit(ast.node, context)

	return result.value, result.error, context if context else Context(file if file else "<program>")