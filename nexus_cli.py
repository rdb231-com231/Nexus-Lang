import cmd
from Interpreter import interpreter
from Interpreter import shell
import sys

class NexusShell(cmd.Cmd):
    prompt = '>> '
    intro = 'Welcome to the Nexus Shell! Please, do: nexus <filename>'

    def do_nexus(self, filename):
        """
        Run the Nexus Script. Use nexus <filename/path> to run the file. For one line code, exit this shell and run: cd Interpreter; py shell.py
        """
        if filename:
            try:
                _, err, ctx = interpreter.run(filename, file=filename)
                if err:
                    print(str(err))
            except Exception as e:
                print(f"Error: {str(e)}")
        else:
            print("Usage: nexus <filename/path>")


    def do_docs(self, args):
        """
        Link of the documentation for the Nexus Script.
        """
        print("Nexus Script Documentation:")
        print("--------------------------------------------------------------")
        print("https://github.com/rdb231-com231/Nexus-Lang/tree/main/Documentation")
        print("--------------------------------------------------------------")
    
    def do_exit(self, args):
        """
        Exit the Nexus Shell.
        """
        print("Exiting the Nexus Shell...")
        sys.exit(0)

NexusShell().cmdloop()