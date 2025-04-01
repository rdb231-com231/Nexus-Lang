import cmd
from Interpreter import interpreter

class NexusShell(cmd.Cmd):
    prompt = '>> '
    intro = 'Welcome to the Nexus Shell! Please, do: nexus <filename>'

    def do_nexus(self, filename):
        """
        Run the Nexus Script. Use nexus <filename/path> to run the file.
        """
        if filename:
            try:
                interpreter.run(filename, file=filename)
            except Exception as e:
                print(f"Error: {str(e)}")
        else:
            print("Please, provide a filename.")
    
    def do_docs(self):
        """
        Link of the documentation for the Nexus Script.
        """
        print("Nexus Script Documentation:")
        print("-------------------------")
        print("https://github.com/rdb231-com231/Nexus-Lang/blob/main/DOCS")

NexusShell().cmdloop()