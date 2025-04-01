from Interpreter import interpreter
import time

reopen = False

def start():
	while True:
			print("Hello! This is the Nexus Shell for one-line code, if you want to run a file directly into the shell, use run(\"filename.nxs\")")
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
			if text == 'help':
				print("Please provide a code following Nexus Syntax Guidelines or use: exit (exits the shell), clear/cls (clears the terminal), close (closes and clears the terminal).")
			result, error = interpreter.run('<stdin>', text)

			if error: print(error.as_string())