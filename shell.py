import basic
import time

reopen = False

while True:
		text = input('Nexus > ')
		if text.strip() == "": continue	
		if text == 'close':
			print('\033c', end='')
			break
		if text == 'exit':
			break
		if text == 'clear' or text == 'cls':
			print('\033c', end='')
			continue
		result, error = basic.run('<stdin>', text)

		if error: print(error.as_string())
		elif result:
			if len(result.elements) == 1: print(repr(result.elements[0])) 
			else: print(repr(result))