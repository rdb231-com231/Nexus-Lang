import cmd
from Interpreter import interpreter
from Interpreter import shell

class NexusShell(cmd.Cmd):
    prompt = '>> '
    intro = 'Welcome to the Nexus Shell! Please, do: nexus <filename>'

    def do_nexus(self, filename):
        """
        Run the Nexus Script. Use nexus <filename/path> to run the file. For one line code, exit this shell and run: cd Interpreter; py shell.py
        """
        if filename:
            try:
                interpreter.run(filename, file=filename)
            except Exception as e:
                print(f"Error: {str(e)}")
        else:
            print("Usage: nexus <filename/path>")
            

    def do_docs(self):
        """
        Link of the documentation for the Nexus Script.
        """
        print("Nexus Script Documentation:")
        print("-------------------------")
        print("https://github.com/rdb231-com231/Nexus-Lang/tree/main/Documentation")

NexusShell().cmdloop()