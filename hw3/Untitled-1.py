#!/usr/bin/env python3
import os
import textwrap
import subprocess

# Trim any leading/trailing newlines
def trim_str(inst):
	if inst[0] == "\n":
		inst = inst[1:]
	if inst[-1] == "\n":
		inst = inst[:-1]
	return inst

def print_dedent(str):
	print(textwrap.dedent(str))

# ****
# text_question
# ****
# 
# the "text_question" type is a question type
# in which user input is matched against a list of correct answers
# and then executed in the shell
#
# specifically:
# 1) the student is asked for input
# 2) this input is directly matched against a set list of "correct" answers
# 3) if input is acceptable, continue; else, repeat
# 4) correct input is executed using "os.system"
#
# the arguments provided to this function are:
# * answers: a list of strings corresponding to correct commands
# * instruction: a string saying what the question is
# * hint: a hint for the users if they get the question wrong

def text_question(answers, instruction, hint):
	instruction = trim_str(instruction)
	hint = trim_str(hint)
	user_input = None

	print_dedent("\n" + instruction + "\n")
	user_input = input("-bash-4.4$ ")

	while user_input not in answers:
		print_dedent(hint + "\n")
		user_input = input("-bash-4.4$ ")

	os.system(user_input)

	return True

# ****
# plaintext_question
# ****
# 
# the "plaintext_question" type is a question type
# in which user input is matched against a list of correct answers
# this answer is NOT executed in the shell
#
# specifically:
# 1) the student is asked for input
# 2) this input is directly matched against a set list of "correct" answers
# 3) if input is acceptable, continue; else, repeat
#
# the arguments provided to this function are:
# * answers: a list of strings corresponding to correct answers
# * instruction: a string saying what the question is
# * hint: a hint for the users if they get the question wrong

def plaintext_question(answers, instruction, hint):
	instruction = trim_str(instruction)
	hint = trim_str(hint)
	user_input = None
	
	print_dedent("\n" + instruction + "\n")
	user_input = input("Write your answer here: ")

	while user_input not in answers:
		print_dedent(hint + "\n")
		user_input = input("Write your answer here: ")

	return True

# ****
# cd_question
# ****
# 
# due to the way child processes are handled, 
# we need a special question type for "cd" commands
#
# the "cd_question" type is a question type
# in which user input is matched against a list of correct answers
# and then the current directory is updated
#
# specifically:
# 1) the student is asked for input
# 2) this input is directly matched against a set list of "correct" answers
# 3) if input is acceptable, continue; else, repeat
# 4) provided answer path is changed to using "os.chdir"
#
# the arguments provided to this function are:
# * answers: a list of strings corresponding to correct "cd" commands
# * path: the correct path that we will want to change directories to
# * instruction: a string saying what the question is
# * hint: a hint for the users if they get the question wrong

def cd_question(answers, path, instruction, hint):
	instruction = trim_str(instruction)
	hint = trim_str(hint)
	user_input = None

	print_dedent("\n" + instruction + "\n")
	user_input = input("-bash-4.4$ ")

	while user_input not in answers:
		print_dedent(hint + "\n")
		user_input = input("-bash-4.4$ ")

	# os.path.expanduser allows us to use shortcuts such as "~"
	os.chdir(os.path.expanduser(path))

	return True

# ****
# command_question
# ****
# 
# the "command_question" type is a question type
# designed to be somewhat more sophisticated than "text_question"
#
# the idea is that there is no "right" answer for many of the 
# things you want to do in UNIX;
# thus, rather than giving this question type a list of "correct" answers,
# you provide it with a function "f" that returns true if what the user did
# was correct, and false otherwise
#
# an example is provided below
#
# Note: I ended up not using this question type, because I could not 
# check beforehand if what the user entered was a correct command;
# rather, whatever they type was executed anyways
# As their incorrect answers might produce unexpected consequences,
# I decided this question type was not worth the risk in a tutorial setting
#
# specifically:
# 1) the student is asked for input
# 2) the input is executed using "os.system"
# 3) if user was correct (as determined by the provided function), continue;
#    otherwise, repeat
#
# the arguments provided to this function are:
# * answers: a list of strings corresponding to correct commands
# * instruction: a string saying what the question is
# * hint: a hint for the users if they get the question wrong

def command_question(f, command, instruction, hint):
	user_input = None
	print(instruction + "\n")
	user_input = input("-bash-4.4$ ")

	try:
		os.system(user_input)
	except:
		pass

	while not f() or command not in user_input:
		print(hint + "\n")
		user_input = input("-bash-4.4$ ")

		try:
			os.system(user_input)
		except:
			pass

	return True

# command_question example:
# this is a "touch filename" question; checks that the file is written using "ls"
#
# command_question(lambda: True if "hello" in os.popen('ls').read() else False,
# 				 "touch",
# 				 "Use touch to create a file called hello", 
# 				 "Hint: touch filename will create a new file with the name filename")
#
# "lambda: True if "hello" in os.popen('ls').read() else False" is
# a lambda function that returns true and false, and 
# it is written in a fashion similar to the ternary
# operator in C so that it easily fits on one line

# *****
# print_command 
# *****
#
# print_command simply executes and prints the given UNIX command
# so that the student can see the command and its output

def print_command(command):
	print("")
	print("-bash-4.4$ " + command)
	os.system(command)

	return True

# *****
# end_of_section
# *****
#
# this command allows users to exit from the tutorial early
# else, the tutorial continues

def end_of_section():
	print("")

	user_input = input("Press \"enter\" to continue or type \"quit\" to exit: ")
	if user_input in ["quit", "exit", "\"quit\"", "q"]:
		exit()
	else:
		return True

# *****
# This is where the code for the tutorials begins
# *****

# *****
# Principle 1, Section 1
# *****
def comp1_sect1():
	os.system("clear")

	print_dedent('''
		Principle 1, Section 1: Navigating in UNIX

		In this section, we will teach you how to navigate between directories
		in UNIX.  Specifically, you will learn how to:

		1) Figure out what directory you are currently in 
		2) List the contents of your current directory 
		3) Change directories
		4) Use additional options when listing contents
		5) Learn shortcuts for navigating in UNIX''')

	end_of_section()

	###
	# pwd
	###
	os.system("clear")

	print_dedent('''
		1) Determining the location of your current directory (pwd):

		In UNIX, at any certain time, you will be inside one certain
		directory, known as your current directory or working directory.  The
		first command we will teach you is the \"pwd\" command, which prints
		out your current directory.  Let's try this out below! ''')

	text_question(["pwd"], 
				  '''
				  Note that the prompt, -bash-4.4$, below indicates that you should
				  enter a UNIX command.  You can execute a command by typing it and then
				  hitting the \"enter\" key.

				  Let's see what directory you're in by typing \"pwd\" (without quotes)''',
		      	  "Hint: just type \"pwd\" (without quotes) and then hit the \"enter\" key")

	print_dedent('''
		Nice job! Your output should look something like 
		/home/accts/NETID where NETID is your Yale NetID. 

		The rightmost directory, \"NETID\", is your current directory.  The
		remaining directories in the output give you the location of this
		\"NETID\" directory.  In this case, the output \"/home/accts/NETID\"
		tells you that \"NETID\" is in a directory called \"accts\", and
		\"accts\" is in a directory called \"home\".

		We often use the term parent directory to denote the directory that
		contains a file or another directory.

		Thus, \"accts\" would be the parent directory of \"NETID\", and
		\"home\" would be the parent directory of \"accts\".  ''')


	print_dedent('''
		P.S. This \"NETID\" directory is a special directory known as your
		home directory (this should not be confused with the directory named
		\"home\" mentioned above).  Every user on a computer has their own
		home directory which contains important user-specific folders such as
		\"Documents\" and \"Desktop\".  ''')

	end_of_section()

	###
	# ls
	###
	os.system("clear")

	print_dedent('''
		2) Listing the contents of your current directory (ls):

		Next, we'll learn about the \"ls\" command.  The \"ls\" command allows
		you to find out what files are in your current directory. ''')

	text_question(["ls"],
		          '''
				  Try typing \"ls\" (without quotes) to see what files are in your current 
				  directory!"''',
				  "Hint: just type \"ls\" (without quotes)")

	print_dedent('''
		Great job! The above output is a list of all the files and directories
		located within your current directory.  Because you are in your home
		directory, \"ls\" just listed the contents of your home directory.
		However, if you then changed directories, \"ls\" will likely give you
		very different results!

		Also, you might have noticed a directory called \"tutorial\" that is
		among the above output.  This directory was created by the tutorial
		script.  We'll teach you how to change directories to the \"tutorial\"
		directory next! ''')
        
	end_of_section()

	###
	# cd
	###
	os.system("clear")

	print_dedent('''
		3) Changing directories (cd):

		Now, we'll learn about the \"cd\" command.  The \"cd\" command allows
		you to move between different directories.  Just use the \"cd
		dirname\" command, where \"dirname\" is the name of the directory you
		are changing to. ''')

	cd_question(["cd tutorial"], 
		        "tutorial",
				'''
				You might remember that \"tutorial\" directory from before.  How 
				would you change directories to that \"tutorial\" directory?''',
				'''
				Hint: \"cd hello\" would change directories to a directory called \"hello\".  
				So how would you move to \"tutorial\"?''')

	print_dedent('''
		Great job! We can check that we've actually moved to the right place
		by using \"pwd\" again. We've done that for you. Take a look!  ''')

	print_command("pwd")

	print_dedent('''
		As you can tell from the output of \"pwd\", we're now in the
		\"tutorial\" directory.  So, now you know how to move between
		directories.  We'll teach you some tips and tricks with \"cd\" in a
		bit!  ''')

	end_of_section()

	###
	# ls options
	###
	os.system("clear")

	print_dedent('''
		4) Using additional options when listing contents

		In addition, many UNIX commands have multiple options.  For instance
		the \"ls\" command allows you to view more info about the files in
		your directory by using the \"-l\" option.  Just enter the \"ls -l\"
		command to view more info about your files.''')

	text_question(["ls -l"], 
				  "Trying typing \"ls -l\" in the terminal",
				  "Hint: just type \"ls -l\" (with no quotes)")

	print_dedent('''
		Nice! As you can see, \"ls -l\" lists a ton of info, such as the last
		time the files were modified.  

		Another commonly used option with \"ls\" is the \"-a\" option. The \"-a\" 
		option allows you to list the hidden files in a directory. 
		Hidden files are files that are not viewable by the user by default; 
		typically they are used to store information such as user preferences. 
		All hidden files in UNIX start with the "." character. Let's try out the 
		\"ls -a\" command below. ''')

	text_question(["ls -a"], 
				  "Trying using the \"ls -a\" command",
				  "Hint: just type \"ls -a\" (with no quotes)")
	
	print_dedent('''
		In the output above, you can see the hidden files "." and "..". These are 
		actually two special hidden directories that are in every directory. 
		In particular, "." corresponds to your current directory and ".." corresponds 
		to the parent directory of your current directory. 

		One last thing, you can even combine multiple options! Check out how you can 
		use both "-a" and "-l" below. ''')

	print_command("ls -al")

	print("\nNice job! Many other UNIX commands also have additional options.")

	end_of_section()

	###
	# cd shortcuts
	###
	os.system("clear")

	print_dedent('''
		5) Learning shortcuts for navigating in UNIX

		There are a lot of useful shortcuts when using \"cd\" to navigate.
		For instance, \"~\" is a shortcut for your home directory, so \"cd ~\"
		will move you to your home directory.  In fact, \"cd\" (with no
		arguments) will also move you to your home directory.

		In addition, you can navigate to your current directory's parent
		directory by using the \"cd ..\" command, and, you can navigate to the
		previous directory you were in, by using the \"cd -\" command.

		Finally, there is a special directory called the root directory.
		This directory is the top-most directory in the file system, and is 
		usually referred to as "/". Thus, you can change directories to the 
		root directory using the "cd /" command. ''')

	cd_question(["cd ~", "cd -", "cd ..", "cd"],
				"~",
				'''
				Now that you know all of these shortcuts, what's a way you can return 
				to the home directory?''',
				"Hint: try one of the shortcuts you've just learned!")

	print_dedent('''
		Great! Let's quickly check that we've actually moved to the right
		place by using \"pwd\" again. Take a look!''')

	print_command("pwd")

	print_dedent('''
		Nice! As you might have noticed, there were a ton of ways to go back
		to your home directory.  You could have used \"cd ~\" or \"cd\" to go
		directly to your home directory.  Alternatively, you could have used
		\"cd ..\" to return to the parent directory of cs201 (which was your
		home directory in this case), or you could have used \"cd -\" to
		return to your previous directory.

		Great job! You've finished Section 1 of Principle 1. ''')

	end_of_section()

# *********
# Principle 1, Section 2
# *********
def comp1_sect2():
	os.system("clear")

	print_dedent('''
		Principle 1, Section 2: Creating, renaming, and deleting files and
		directories

		In this section, we will teach you how to create, rename, and delete
		files and directories. Specifically, you will learn how to:

		1) Create new directories 
		2) Create new files and update existing files' timestamps 
		3) Copy files and directories " 
		4) Rename and move files and directories"
		5) Delete files"
		6) Delete directories''')

	end_of_section()

	###
	# mkdir
	###
	os.system("clear")

	print_dedent('''
		1) Creating new directories (mkdir):

		First, we'll teach you how to create a new directory. But let's first
		change directories to the \"tutorial\" directory.''')

	cd_question(["cd tutorial"], 
		    	"tutorial",
				"Remember how to do that? ",
				"Hint: \"cd hello\" would change directories to a directory called \"hello\". ")


	print_dedent('''
		Nice job!  Now, in order to create a new directory, you will want to
		use the \"mkdir\" command.  Just use the \"mkdir dirname\" command,
		where \"dirname\" is the name of the new directory you want to create.''')

	text_question(["mkdir hello"], 
				  "How would you make a new directory called \"hello\"? ",
				  '''
				  Hint: \"mkdir bye\" would create a new directory called \"bye\".   So how 
				  would you create a directory called \"hello\"?''')

	print_dedent('''
		Great job! You can check that you've actually made the new directory
		by using the \"ls\" command.  We've done that for you. Take a look!''')

	print_command("ls")

	print_dedent('''
		As you can tell from the output of \"ls\", we're now made a new
		directory called \"hello\" in the \"tutorial\" directory.''')

	end_of_section()

	###
	# touch
	###
	os.system("clear")

	print_dedent('''
		2) Creating new files and updating existing files' timestamps (touch):

		Now we'll teach you how you can create new files.  You can use the
		\"touch filename\" command to create a new file, where \"filename\" is
		the name of the new file you want to create.  This will create an
		empty new file, as long as you don't already have a file with the name
		\"filename\".''')

	text_question(["touch banana"], 
				  "How would you make a new file called \"banana\"? ",
				  '''
				  Hint: \"touch bye\" would create a new file called \"bye\".  So how would 
				  you create a file called \"banana\"?''')

	print("\nNice! Let's use \"ls -l\" to make sure that that worked!")

	print_command("ls -l")

	print_dedent('''
		Great job! As you can see, you've successfully made a new file called
		\"banana\".''')

	print_dedent('''
		Now, you will learn about another use of the \"touch\" command.  If
		you use the \"touch filename\" command and the file \"filename\"
		already exists, you will update the timestamp of the existing file
		instead of creating a new file.  Take note of the timestamp of the
		\"apple\" file in the above output, and then try out this usage of
		\"touch\" for yourself below.''')
		   
	text_question(["touch apple"], 
				  "How would you update the timestamp of \"apple\"? ",
				  '''
				  Hint: you can use \"touch filename\" to update the timestamp of 
				  \"filename\" if that file already exists.''')

	print("\nLet's just do \"ls -l\" one more time to see if that worked.")

	print_command("ls -l")

	print_dedent('''
		Great job, as you can see above, the timestamp has indeed been updated
		to right now.  Now you know how to use \"touch\" to create new files
		and update the timestamps of old files.''')

	end_of_section()

	###
	# cp
	###
	os.system("clear")

	print_dedent('''
		3) Copying files and directories (cp):

		Now, you'll learn how to use the \"cp\" command to copy files.  Just
		type the \"cp filename copyname\" command to create a new file, where
		\"filename\" is the name of the file are copying and \"copyname\" is
		the name of the copy you want to create.  Let's try this out below!''')

	text_question(["cp apple pear"], 
				  "How would you create a copy of \"apple\" called \"pear\"? ",
				  '''
				  Hint: the command \"cp banana plum\" would create a copy of \"banana\" 
				  called \"plum\".''')

	print_dedent('''
		Nice! Let's quickly run \"ls\" to check if you've made a file called
		\"pear\".''')

	print_command("ls")

	print_dedent('''
		Good job! To copy directories, you’ll need to use the \"-r\" option.
		This option will copy the files in a directory and then the directory
		itself.  Similar to before, type \"cp -r dirname copyname\" command to
		create a new directory, where \"dirname\" is the name of the directory
		are copying and \"copyname\" is the name of the copy you want to
		create.''')

	text_question(["cp -r comp1 comp1_copy"], 
				  '''
				  How would you create a copy of the directory \"comp1\" called 
				  \"comp1_copy\"?''',
				  '''
				  Hint: the command \"cp -r comp1 plums\" would create a copy of \"comp1\" 
				  called \"plums\".''')

	print("\nCool! Let's quickly run \"ls\" again to check")

	print_command("ls")

	print_dedent('''
		Nice job copying! Next, we'll learn about renaming and moving files
		and directories. ''')

	end_of_section()

	###
	# mv
	###
	os.system("clear")

	print_dedent('''
		4) Renaming and moving files and directories (mv):

		You can use the \"mv\" command to rename files and directories.
		Typing the \"mv name1 name2\" command will change the name of the
		given file/directory from \"name1\" to \"name2\".  Let's try this out
		below!''' )

	text_question(["mv pear peach"], 
				  "How would you rename \"pear\" to \"peach\"? ",
				  '''
				  Hint: the command \"mv apple crabapple\" would rename \"apple\" to 
				  \"crabapple\".''')

	print("\nNice! Let's quickly run \"ls\" to check. ")

	print_command("ls")

	print_dedent('''
		Great job! You can also use the \"mv\" command to move files and
		directories.  Typing the \"mv name1 dirname\" command would move the
		file/directory \"name1\" from its current location to the directory
		\"dirname\".  Let's try this!''')

	text_question(["mv comp1_copy comp1"], 
				  "How would you move \"comp1_copy\" to the directory \"comp1\"? ",
				  '''
				  Hint: the command \"mv apple hello\" would move \"apple\" from your 
				  current directory to the directory \"hello\".''')

	print("\nOk, let's quickly run \"ls comp1\" to check the contents of \"comp1\" ")

	print_command("ls comp1")

	print_dedent('''
		Nice job! As you can see, you've successfully moved \"comp1_copy\" to
		be inside the \"comp1\" directory.''')

	end_of_section()

	###
	# rm
	###
	os.system("clear")

	print_dedent('''
		5) Deleting files (rm):

		You can use the \"rm filename\" command to delete a file called
		\"filename\".  Be careful with what files you delete with this command
		because you won't be asked for confirmation!  If you're worried about
		deleting the wrong file, you can use the \"-i\" option. This will make
		\"rm\" ask you for confirmation.  You would then type \"yes\" or
		\"no\" to confirm the deletion.''')

	text_question(["rm -i peach"], 
				  '''
				  How would you delete the file \"peach\", while using the \"-i\" 
				  option to check for confirmation?''',
				  '''
				  Hint: the command \"rm -i apple\" would delete \"apple\" from 
				  your current directory after asking for confirmation.''')

	print("\nNice! Next you'll learn how to delete directories.")

	end_of_section()

	###
	# rmdir
	###
	os.system("clear")
	print_dedent('''
		6) Deleting directories (rmdir, rm -r):

		You can use the \"rmdir dirname\" command to delete an empty directory 
		called \"dirname\".''')

	text_question(["rmdir hello"], 
				  "How would you delete the directory \"hello\"? ",
				  '''
				  Hint: the command \"rmdir bye\" would delete the directory \"bye\" as 
				  long as \"bye\" was empty.''')

	print("\nCool, let's quickly check it worked by running \"ls\"")

	print_command("ls")

	print_dedent('''
		Nice job, you've deleted the directory!  If you want to delete a
		directory that isn't empty, you can use the \"-r\" option with the
		\"rm\" command.  For instance, the \"rm -r comp1\" command would
		delete the \"comp1\" directory and all files inside of it.''')

	text_question(["rm -r comp1/comp1_copy"], 
				  '''
				  How would you delete the \"comp1_copy\" (which is now located in 
				  \"comp1\")?  You can reference the directory now as \"comp1/comp1_copy\".''',
				  "Hint: try the command \"rm -r comp1/comp1_copy\"")

	print("\nOnce again, let's quickly check it worked by running \"ls comp1\"")

	print_command("ls comp1")

	print("\nNice job! That's the last part of this section!")

	end_of_section()

# *********
# Principle 1, Section 3
# *********
def comp1_sect3():
	os.system("clear")

	print_dedent('''
		Principle 1, Section 3: Printing/viewing useful information

		In this section, we will teach you how to print useful information to
		the terminal.  Specifically, you will learn how to:

		1) Print out the contents of files 
		2) Print out just the beginning/end of a file 
		3) Scroll through files 
		4) View help information about UNIX commands ''')

	end_of_section()

	###
	# cat
	###
	os.system("clear")

	print_dedent('''
		1) Printing out the contents of a file:

		You can use the \"cat filename\" command to print out a file called
		\"filename\" to the terminal. Let's try it!''')

	text_question(["cat apple"], 
				 "How would you print out the \"apple\" file to the terminal? ",
				 "Hint: \"cat pear\" would print out a file called \"pear\". ")

	print_dedent('''
		Ok great! There are a few more text files in the \"comp1\" directory. 
		Let's check them out!''')

	cd_question(["cd comp1"], 
				"comp1",
				"First, let's \"cd\" to the \"comp1\" directory ",
				"Hint: type \"cd comp1\"")

	print_dedent('''
		Nice! The \"cat\" command also allows you to print out multiple files
		at once if you provide it with two or more files as command line
		arguments.  For instance, the \"cat shopping_list favorite_colors\"
		command will print out both files. See the example below:''')

	print_command("cat shopping_list favorite_colors")
	
	text_question(["cat shopping_list favorite_colors favorite_fruit"], 
				  '''
				  Now, how would we print out \"shopping_list\", then \"favorite_colors\", 
				  and then \"favorite_fruit\", in that order? ''',
				  '''
				  Hint: \"cat filename1 filename2 filename3\" would print out \"filename1\", 
				  \"filename2\", and then \"filename3\".''')

	print("\nGreat job!")

	end_of_section()

	###
	# head/tail
	###
	os.system("clear")

	print_dedent('''
		2) Printing out the beginning/end of a file (head/tail): 

		In some cases, you might not want to print out an entire file.
		Rather, you might want to just print out the beginning or end of a
		file. In these cases, you would use the \"head\" and \"tail\"
		commands.

		The \"head\" and \"tail\" commands allow you to print out the first
		few or last few lines of a file, respectively.  By default, these
		commands print out the first/last 10 lines of a file.  For instance,
		the \"head long_textfile\" command would print out the first 10 lines
		of the \"long_textfile\" file.  In addition, you can use the \"-n\"
		option to set the amount of lines you want to be printed.  As you can
		see below the \"tail -n 5 long_textfile\" commands prints out the last
		5 lines of \"long_textfile\".''')

	print_command("tail -n 5 long_textfile")

	text_question(["head -n 15 long_textfile"], 
				 "How would you print out the first 15 lines of the file \"long_textfile\"? ",
				 '''
				 Hint: \"head -n 6 pear\" would print out the first 6 lines of a file 
				 named \"pear\".''')

	print("\nGreat job!")

	end_of_section()

	###
	# more/less
	###
	os.system("clear")

	print_dedent('''
		3) Scroll through files (more/less): 

		As opposed to the \"cat\", \"head\", and \"tail\" commands which print
		out a file, you can use the \"more\" and \"less\" commands to scroll
		through a file.  The main difference between \"more\" and \"less\" is
		that you cannot scroll backwards in more, while you can scroll
		backwards in less.

		You can use the \"more filename\" or \"less filename\" commands to
		scroll through a file.  These commands will open a file viewer in the
		terminal.  Once you are scrolling through a file, you can use the
		\"q\" key to exit from the viewer.''')

	text_question(["less long_textfile"], 
				  '''
				  How would you use \"less\" to scroll through the file \"long_textfile\"? 
				  Remember to use the \"q\" key to exit! ''',
				  "Hint: type \"less long_textfile\" and then \"q\" to exit. ")

	print("\nGreat job!")

	end_of_section()

	###
	# man
	###
	os.system("clear")

	print_dedent('''
		4) View help information about UNIX commands (man): 

		You can use the \"man\" command to print out help information about a
		UNIX command you are unfamiliar with.  For instance, if you wanted to
		learn more about the \"ls\" command, you would use the \"man ls\"
		command.  Similar to \"more\"/\"less\", \"man\" will open up a viewer
		inside the terminal window, which you can use the \"q\" key to exit.

		Great job! You've finished Principle 1 of the UNIX tutorial!''')

	input("\nPress enter to close the tutorial.")

# *********
# Principle 1, Start
# *********
def comp1():
	os.system("clear")

	print_dedent('''
		Welcome to Principle 1 of our UNIX tutorial!

		In this Principle, you're going to learn the basics of UNIX.  This
		Principle is broken into 3 sections.  In particular, you'll learn how
		to navigate UNIX, create/rename/delete files and directories, and
		print useful information to the terminal.

		Let's get started!''')

	end_of_section()

	comp1_sect1()
	comp1_sect2()
	comp1_sect3()

# *********
# Principle 2, Section 1
# *********
def comp2_sect1():
	os.system("clear")

	print_dedent('''
		Principle 2, Section 1: Streams

		In this section, we will teach you how to use streams in
		UNIX. Specifically, you will learn how to:

		1) Redirect stdout to create a new file 
		2) Redirect stdout to append a new file 
		3) Redirect files to stdin 
		4) Pipe stdout from one command to be stdin of another command''')

	end_of_section()

	###
	# Intro
	###
	os.system("clear")

	print_dedent('''
		Introduction to streams:

		In UNIX, all programs have streams associated with them.  These
		streams correspond to the inputs (of text, etc.) to a program and the
		outputs of a program. Usually we refer to the standard input stream of
		a program as stdin, and the standard output of a program as
		stdout. There is also a standard error output stream of a program
		known as stderr.  In this section we will talk about how to redirect
		streams to create new files and combine various UNIX commands
		together. ''')

	end_of_section()

	###
	# >
	###
	os.system("clear")

	print_dedent('''
		1) Redirecting stdout to create a new file (echo, >):

		First, we will introduce you to a new command \"echo\". This command
		simply takes some command line arguments and prints them to the
		terminal. Let's try this out! ''')

	text_question(["echo hello world"],
		          "Let's see what happens if you type \"echo hello world\". ",
		          "Hint: Type \"echo hello world\" ")

	print_dedent('''
		As you can see, this command just printed \"hello world\" to the
		command line. 

		This example matches what we have seen so far.  By default, the
		standard output (stdout) of a program is printed to the terminal.  In
		fact, all of the commands that you have used (including \"ls\", \"cat\", 
		and now \"echo\") have just printed their output to the terminal.

		So how can we use stdout differently?  Well, instead of printing the
		output of a command to the terminal, we can redirect that output to
		create a new file.  We do this by using \">\", the output redirection
		operator.  When you use the \"> filename\" operator in a command, the
		output that would have been printed to the terminal is instead
		redirected to be the contents of a new file called \"filename\".
		Consider the example below: ''')

	print_command("echo hello world > hello.txt")

	print_command("cat hello.txt")

	print_dedent('''
		As you can see above, the \"echo hello world > hello.txt\" command
		created a new file called \"hello.txt\" that contained \"hello
		world\". Now it's your turn! ''')

	text_question(["echo apple orange banana > fruit.txt", 
				   "echo \"apple orange banana\" > fruit.txt"], 
				  '''
				  How would you create a file called \"fruit.txt\" that contained the
				  string \"apple orange banana\"? ''',
                  '''
                  Hint: \"echo hello world > fruit.txt\" would create a new file called 
                  \"fruit.txt\" that contained \"hello world\".''')

	print("\nNice! Let's first check that this worked by using \"cat\".")

	print_command("cat fruit.txt")

	print_dedent('''
		Nice job! You can also use the \">\" operator to redirect the stdout
		of any of the commands you have learned to use before. ''')

	end_of_section()

	###
	# >>
	###
	os.system("clear")

	print_dedent('''
		2) Redirecting stdout to append to an existing file (>>):

		Note that the \"> filename\" operator will always create a new file,
		even if \"filename\" already exists. For instance, consider the
		following sequence of commands: ''')

	print_command("cat apple")

	print_command("echo hi world > apple")

	print_command("cat apple")

	print_dedent('''
		As you can see, the original contents of \"apple\" (\"Hello, world!\")
		has been replaced with \"hi world\".

		Thus, we will now introduce you to the \">>\" operator. The \">>\"
		operator allows you to redirect stdout to append to the end of an
		existing file, rather than replacing the file.  Let's try using the
		\">>\" operator. ''')

	text_question(["echo kiwi mango >> fruit.txt"],
		          "Let's see what happens if you type \"echo kiwi mango >> fruit.txt\". ",
		          "Hint: Type \"echo kiwi mango >> fruit.txt\" ")

	print("\nNow, let's take a look at \"fruit.txt\"")

	print_command("cat fruit.txt")

	print_dedent('''
		Nice job! As you can see, by using the \">>\" operator, the output is
		appended to an existing file, rather than replacing the file
		altogether. ''')

	end_of_section()

	###
	# <
	###
	os.system("clear")
	
	print_dedent('''
		3) Redirecting files to stdin (wc, <):

		The \"wc\" command allows you to count the number of words in a file
		or stream. There are two ways to use the \"wc\" command.  First, you
		can call \"wc\" with command line arguments to count the number of
		words in the given files.  For instance, you can use the \"wc
		filename\" command to count the number of words in \"filename\". ''')

	text_question(["wc fruit.txt"],
				  "How would you count the number of words in the \"fruit.txt\" file?",
				  "Hint: Type the \"wc fruit.txt\" command")

	print_dedent('''
		The above output lists the number of lines, words, and characters, in
		that order.  You can also use \"wc\" by providing it with input from
		stdin.  By default, commands will take stdin from what the user types
		in the terminal. ''')

	text_question(["wc"],
				  '''
				  To see this in action, just type the \"wc\" command. As you have
				  provided no arguments, the command will require you to then provide it
				  with input. Simply type what you want to count the words of, and then
				  type CTRL+D, the EOF character.''',
				  "Hint: Type \"wc\" command. Then enter some text and click CTRL+D.")

	print_dedent('''
		As you can see, when you use \"wc\" without arguments, the command
		counts the number of words provided by stdin.  However, instead of
		typing the words you want to be counted by \"wc\", you could use the
		\"<\" operator to redirect a file to stdin.  For instance, \"wc <
		filename\" would call the \"wc\" command, with \"filename\" redirected
		to stdin. ''')

	text_question(["wc < fruit.txt"],
		          '''
				  How could you use \"wc\" to count the number of words in \"fruit.txt\", 
				  by using the \"<\" operator to redirect \"fruit.txt\" to stdin? ''',
		          "Hint: Type \"wc < fruit.txt\" ")

	print_dedent('''
		Great job!  You might be wondering what the difference is between \"wc
		< fruit.txt\" and \"wc fruit.txt\".  In reality, the end result is
		largely the same, although there is a slight difference in the two
		outputs.

		In the end, it turns out that using (or not using) the \"<\" operator
		with many commands produces a negligible difference.  However,
		understanding how the \"<\" operator works is important for
		understanding our next topic: the pipe.''')

	end_of_section()

	###
	# |
	###
	os.system("clear")

	print_dedent('''
		4) Piping stdout from one command to be stdin of another command (|):

		The pipe operator \"|\" is a very powerful tool in UNIX. Piping allows
		you to take redirect stdout from one command to be the stdin of
		another command. This essentially allows you to combine commands.

		For instance, let's say you wanted to see how many words were in the
		output of \"ls\". You can already do this with the tools you have
		learned about.  First, let's create a new file with the output from
		\"ls\". ''')

	text_question(["ls > ls_output"],
				  '''
				  How would you redirect the stdout of \"ls\" to create a file called 
				  \"ls_output\"?''',
				  "Hint: type \"ls > ls_output\"")

	print_dedent('''
		Next, you would just count the words in \"ls_output\" by using the \"wc\" 
		command.''')

	text_question(["wc ls_output", "wc < ls_output"],
				  "How would you use \"wc\" to count the words in \"ls_output\"?",
				  "Hint: type \"wc < ls_output\"")

	print_dedent('''
		Great job!  So this approach clearly works, but it involved multiple
		steps and creating an extra file.  You could use the pipe operator
		\"|\" to do this in one step.  Look at the example below: ''')

	print_command("ls | wc")

	print_dedent('''
		The above example works by piping the stdout of \"ls\" directly into
		the stdin of \"wc\". As you can see, piping can be very powerful and
		very easy to use. Now it's time to try it out for yourself! ''')


	text_question(["cat apple banana | wc"],
				  '''
				  How could you use a pipe to count the number of words in the \"apple\"
				  and \"banana\" files?  Remember that \"cat filename1 filename2\" prints 
				  out the contents of the two files. ''',
                      
				  "Hint: Type the \"cat apple banana | wc\" command")

	print_dedent('''
		Great job!  Next, you'll learn about wildcards in UNIX. ''')

	end_of_section()

# *********
# Principle 2, Section 2
# *********
def comp2_sect2():
	os.system("clear")

	print_dedent('''
		Principle 2, Section 2: Wildcards

		In this section, we will teach you how to use single character and
		multiple character wildcards. ''')

	end_of_section()

	###
	# ?
	###
	os.system("clear")

	print_dedent('''
		1) Single character wildcard (?):

		In UNIX, wildcards allow you to easily call commands with multiple
		files as arguments.  In this section, we will show you how to use
		wildcards to delete many similarly named files.''')

	cd_question(["cd comp2"],
				"comp2",
				"First, let's \"cd\" to the \"comp2\" directory.",
				"Hint: type \"cd comp2\"")

	text_question(["ls"],
				  "Next, let's list the contents of this directory using \"ls\".",
				  "Hint: type \"ls\"")

	print_dedent('''
		Now, let's say we wanted to delete all of the files in this directory.
		One shortcut is that \"rm\" can take multiple arguments.  For
		instance, we could delete multiple files with the command \"rm file1
		file2 file3\" and so on. ''')

	text_question(["rm file1.txt file2.txt file3.txt"],
				  '''
				  How would we delete \"file1.txt\", \"file2.txt\", and \"file3.txt\" using 
				  a single \"rm\" command?''',
				  "Hint: type \"rm file1.txt file2.txt file3.txt\" ")

	print("\nNice! Let's \"ls\" again.")

	print_command("ls")

	print_dedent('''
		As you can see, we deleted those 3 files.  However, typing out the
		name of all the files we want to delete can be pretty tedious.  Let's
		say we now want to delete \"file4.txt\", \"file5.txt\", and
		\"file6.txt\".  You might notice how these files all have similar
		names.  In fact, they all only differ by one character.

		Thus, we can now use \"?\", the single character wildcard.  When used
		in a filename, \"?\" stands for any single character.  By using \"?\",
		we can refer to these three file as \"file?.txt\".  We can then use
		\"file?.txt\" in a command to refer to all of these files. Let's try
		this out! ''')

	text_question(["rm file?.txt"],
				  '''
				  How would we delete \"file4.txt\", \"file5.txt\", and \"file6.txt\" using 
				  the \"?\" wildcard in a command?''',
				  "Hint: type \"rm file?.txt\" ")

	print("\nCool, let's \"ls\" to check that that worked.")

	print_command("ls")

	print("\nNice job! Next, we'll learn about the multiple character wildcard. ")

	end_of_section()

	###
	# *
	###
	os.system("clear")

	print_dedent('''
		2) Multiple character wildcard (*):

		Now, we will show you how to use the multiple character wildcard to
		delete many similarly named files.  We want to now delete the
		\"hw.rkt\", \"hw1.rkt\", \"hw11.rkt\", and \"hw111.rkt\" files.
		However because these filenames differ by more than one character,
		using the \"?\" wildcard will not be adequate.  Instead, we can use
		the \"*\" wildcard which represents any string of 0 or more
		characters.  Thus, \"hw*.rkt\" would refer to any file that started
		with \"hw\" and ended with \".rkt\". ''')

	text_question(["rm hw*.rkt"],
				  '''
				  How would we delete \"hw.rkt\", \"hw1.rkt\", \"hw11.rkt\", and \"hw111.rkt\" 
				  using the \"*\" wildcard?''',
				  "Hint: type \"rm hw*.rkt\" ")

	print("\nLet's \"ls\" again to check if that worked.")

	print_command("ls")

	print("\nNice job! We deleted those 4 files.")

	text_question(["rm *.pdf"],
				  '''
				  Now that you know about the \"*\" wildcard, how would we delete \"a.pdf\", 
				  \"asdf.pdf\", \"dfdafs.pdf\", and \"asdfasd.pdf\"?''',
				  "Hint: type \"rm *.pdf\" ")

	print("\nLet's \"ls\" once more to check.")

	print_command("ls")

	print("\nNice! You deleted all the \".pdf\" files. ")

	text_question(["rm *"],
				  ("Finally, how would we delete all of the files in this directory\"?"),
				  "Hint: type \"rm *\" ")

	print("\nLet's \"ls\" one last time.")

	print_command("ls")

	print_dedent('''
		Nice job! That last command deleted every file left in the
		directory. You've finished the second Principle! ''')

	input("\nPress enter to close the tutorial.")

# *********
# Principle 2, Start
# *********
def comp2():
	os.system("clear")
	os.chdir(os.path.expanduser("~/tutorial"))

	print_dedent('''
		Welcome to Principle 2 of our UNIX tutorial!

		In this Principle, you're going to learn about streams, and how you
		can use streams to create new files and link together multiple UNIX
		commands.  You'll also learn how to use wildcards in UNIX.  Let's get
		started!''')

	end_of_section()

	comp2_sect1()
	comp2_sect2()

# *********
# Principle 3, Section 1
# *********
def comp3_sect1():
	os.system("clear")
	os.chdir(os.path.expanduser("~/tutorial/comp3"))

	print_dedent('''
		Principle 3, Section 1: Getting more information about files

		In this section, we will teach you how to get more information about
		files.  Specifically, you will learn how to:

		1) Test for differences between two files 
		2) Search files or streams for a particular string 
		3) Determine the file type of a file ''')

	end_of_section()

	###
	# diff
	###
	os.system("clear")

	print_dedent('''
		1) Testing for differences between two files (diff):

		First, we will introduce you to a new command \"diff file1 file2\".
		This command allows you to check for differences between two files!
		The command will go through the files line by line and print out any
		differences.

		Before, we test for the differences between \"file1.txt\" and
		\"file2.txt\", let's first print the contents of the two files: ''')

	print_command("cat file1.txt")

	print_command("cat file2.txt")

	text_question(["diff file1.txt file2.txt"],
		          '''
		          Now, let's check for the differences between \"file1.txt\" and \"file2.txt\" 
		          using \"diff\".''',
		          "Hint: Type \"diff file1.txt file2.txt\" ")

	print_dedent('''
		As you can see, this command printed out the lines in which the files
		differ.  Note that the \">\" in the output means that some line is
		present in \"file2.txt\" but missing in \"file1.txt\", while the \"<\"
		means that some line is present in \"file1.txt\" but missing in
		\"file2.txt\". ''')

	end_of_section()

	###
	# grep
	###
	os.system("clear")

	print_dedent('''
		2) Searching files or streams for a particular string (grep):

		Next, we will introduce you to the \"grep\" command.  This command
		allows you to search a file or stream for a particular string, and
		prints out all the lines that contain that string.  To search a file,
		use the command \"grep string filename\" where \"string\" is the
		string you are searching for. ''')

	text_question(["grep world file1.txt", "grep \"world\" file1.txt", "grep 'world' file1.txt"],
		          "How would you search for the string \"world\" in \"file1.txt\". ",
		          "Hint: Type \"grep world file1.txt\" ")

	print_dedent('''
		Nice job!

		You can also pipe input into \"grep\" from a previous command. For
		instance, if you wanted to find all the lines in the \"file1.txt\" and
		\"file2.txt\" files that had \"world\" in them, you could use this
		command: ''')

	print_command("cat file1.txt file2.txt | grep \"world\"")

	print_dedent('''
		In addition, you could use the \"-n\" option to add line numbers to
		the output, or the \"--color\" to color the matched pattern in the
		output. ''')

	end_of_section()

	###
	# file
	###
	os.system("clear")

	print_dedent('''
		3) Determining the file type of a file (file):

		Next, the \"file\" command allows you to determine the file type of a
		file.  Try this out below! ''')

	text_question(["file *"],
		          '''
		          How would you determine the file type of all the files in directory? 
		          (Remember the \"*\" wildcard).''',
		          "Hint: Type \"file *\" ")

	print("Nice job! ")

	end_of_section()

def comp3_sect2():
	# *********
	# Component 3, Section 2
	# *********
	os.system("clear")

	os.chdir(os.path.expanduser("~/tutorial/comp3"))

	print_dedent('''
		Principle 3, Section 2: Miscellaneous UNIX commands

		In this section, we will teach various UNIX commands that will give
		you some information about your computer.  Specifically, you will
		learn how to:

		1) Print information about yourself 
		2) Print information about other users 
		3) Print information about the computer 
		4) Print information about the file sizes and quotas 
		5) Print out more information about commands ''')

	end_of_section()

	###
	# whoami, id
	###
	os.system("clear")

	print_dedent('''
		1) Printing out information about yourself (whoami, id):

		The \"whoami\" and \"id\" commands print out information about your
		user.''')

	text_question(["whoami"],
		          "The \"whoami\" command prints out your username. Try this out! ",
		          "Hint: Type \"whoami\" ")

	print_dedent('''
		The \"id\" command is a bit more complicated; it prints out the id
		numbers related to your username and group, and then it prints out all
		groups that you are in. ''')

	text_question(["id"],
		          "Try typing the \"id\" command. ",
		          "Hint: Type \"id\" ")

	print("Nice job!")

	end_of_section()

	###
	# who, w, last, finger
	###
	os.system("clear")

	print_dedent('''
		2) Printing out information about other users (who, w, last, finger):

		There are several UNIX commands that allow you to print out
		information about other users on your computer. ''')

	text_question(["who"],
		          ''' 
				  For instance, the \"who\" command prints out the username of all the
				  people logged into your computer as well as what time they logged
				  in. Try this out! ''',
		          "Hint: Type \"who\" ")

	text_question(["w"],
		          '''
				  On the other hand, the \"w\" command prints out the same information
				  as \"who\" but also prints out what programs all the users are currently 
				  running. Try this out below! ''',
		          "Hint: Type \"w\" ")

	print_dedent('''
		Furthermore, the \"last\" command prints out the last few users who
		logged into the computer. You can use the \"-n\" option to choose the
		number of lines to print out, and you can use the \"last username\"
		command to see the last times a certain user logged in. ''')

	text_question(["last -n 10"],
		          "How would you print out the last 10 users who logged in?",
		          "Hint: Type \"last -n 10\" ")

	print_dedent('''
		Finally, the \"finger\" command prints out some information about the
		people logged onto your computer, such as a person’s office number and
		office phone number (if known). The \"finger username\" command can
		also be used to list information about some particular user, such as
		their preferred shell and the location of their home directory. ''')

	text_question(["finger"],
		          "Try typing the \"finger\" command. ",
		          "Hint: Type \"finger\" ")

	print("Nice job!")

	end_of_section()

	###
	# uptime, uname, lsb_release
	###
	os.system("clear")

	print_dedent('''
	3) Printing out information about the computer (uptime, uname, lsb_release):

	There are several UNIX commands that allow you to print out
	information about your computer. ''')

	text_question(["uptime"],
		          '''
				  The \"uptime\" command prints out how long the computer has been up
				  since it was last restarted, as well as the number of current users. 
				  Try this out below. ''',
		          "Hint: Type \"uptime\" ")

	text_question(["uname"],
                  ''' 
				  The \"uname\" command simply prints out what operating systems you are
				  currently running. On Linux computers such as the Zoo, the output
				  would be \"Linux\" while Macintosh computers run a version of UNIX
				  called \"Darwin\". Try this out. ''',
		          "Hint: Type \"uname\" ")

	print_dedent('''
		Finally, the \"lsb_release\" command prints out some information about
		the currently installed UNIX operating system and its packages. You
		can also use the \"-a\" option to get even more information. See some
		examples below: ''')

	print_command("lsb_release")

	print_command("lsb_release -a")

	print("Nice job!")

	end_of_section()

	###
	# du, quota
	###
	os.system("clear")

	print_dedent('''
		4) Printing out information about the file sizes and quota (du, quota):

		There are a couple UNIX commands that allow you to print out
		information about file sizes and quota.

		The \"du\" command prints out the size of some directory or file.  If
		you do not provide any arguments, the \"du\" command will print out
		the size of the current directory.  On the other hand, \"du name\"
		will print out the size of whatever directory or file was provided.''')

	text_question(["du", "du ."],
		          "How would you get the size of our current directory?",
		          "Hint: Type \"du\" ")

	print_dedent('''
		Nice! The \"quota\" command is supposed to print out the amount of
		disk space allocated to your account.  However, it does not seem to
		work on the Zoo. ''')

	print_command("quota")

	end_of_section()

	###
	# info
	###
	os.system("clear")

	print_dedent('''
		4) Printing out more information about commands (info):
			
		The \"info\" command is similar to the \"man\" command in that it
		prints out information about how to use a particular command. The two
		manuals were written by different groups, so if you have trouble
		understanding documentation from one of these commands, it might be
		worth it to check out the other command. ''')

	text_question(["info ls"],
		          '''
				  How would you get info about the \"ls\" command? Similar to \"man\",
				  you exit the viewer using the \"q\" key''',
		          "Hint: Type \"info ls\" ")

	print("\nNice job! You've finished Principle 3!")

	input("\nPress enter to close the tutorial.")

def comp3():
	os.system("clear")
	os.chdir(os.path.expanduser("~/tutorial"))

	print_dedent('''
		Welcome to Principle 3 of our UNIX tutorial!

		In this Principle, you're going to get more information about files
		and some miscellaneous UNIX commands.  Let's get started!''')

	end_of_section()

	comp3_sect1()
	comp3_sect2()

def comp4():
	os.system("clear")
	os.chdir(os.path.expanduser("~/tutorial"))

	print_dedent('''
		Welcome to Principle 4 of our UNIX tutorial!

		In this Principle, you're going to learn how to change file
		permissions and file ownership.  Let's get started!''')

	end_of_section()

	###
	# intro
	###
	os.system("clear")

	print_dedent('''
		Introduction to file permissions: 

		Remember all of that info that \"ls -l\" printed out?  Take a look
		below: ''')

	print_command("ls -l")

	print_dedent('''
		The leftmost column above of dashes, r's, w's, and x's tells us what
		the access rights are for each file.  The first character tells us if
		the corresponding item is a file (-) or directory (d).  The remaining
		9 characters are made up of 3 triplets, these give us the read (r),
		write (w), and execute (x) access rights (or lack thereof) for the
		owner, the group, and the world, respectively.  If any of these
		characters is a dash (-), this signifies that that particular access
		right is not given.

		For example, the row of the leftmost column corresponding to the file
		\"apple\" is \"-rwxrw-r--\".  That first \"-\" shows that the file is
		not a directory.  The first triplet \"rwx\", corresponding to the file
		owner, signifies that the owner has read (r), write (w), and execute
		(x) permissions.

		The second triplet \"rw-\", corresponding to the group, signifies that
		the group has read (r) and write (w) access rights, but does not have
		execute (-) permissions.

		Finally, the last triplet \"r--\", corresponding to the world,
		signifies that the world has read (r) access rights, but does not have
		write (-) or execute (-) permissions. ''')

	plaintext_question(["yes", "Yes", "y"],
					   '''
					   Look at the file \"test\" in the output from \"ls -l\" above. Does 
					   the world have read permissions? (Yes/No)''',
					   "Hint: type either \"Yes\" or \"No\"")

	print("\nNice job! Next, you'll learn about how to change permissions. ")

	end_of_section()

	###
	# chmod
	###
	os.system("clear")

	print_dedent('''
		Changing file permissions (chmod): 

		We will now introduce you to the \"chmod\" command which will allow
		you to change the file permissions of a file.

		The \"chmod\" command is most commonly used in \"octal mode\".  To
		understand how this works, consider the file permissions for the file
		\"apple\", \"rwxrw-r--\".  We can interpret each of these 9 characters
		as a binary digit, where every r, w, and x corresponds to the digit 1,
		and every - corresponds to the digit 0.  Thus, we could rewrite 

		\"rwx rw- r--\" as \"111 110 100\".  

		We can then go one step further by interpreting each triplet of bits
		as a one digit base 8 (octal) number.

		For instance, the binary number \"111\" corresponds to the number
		\"7\" in octal.  Likewise, \"110\" corresponds to \"6\" and \"100\"
		corresponds to \"4\".  Thus, we have shown how we can rewrite
		\"rwxrw-r--\" as \"764\".  (Note: if you are having trouble
		understanding octal, consider that converting each of these 3-digit
		binary numbers to an octal digit is essentially the same as converting
		them to a decimal digit).

		These octal numbers can then be used in a \"chmod\" command.  To
		change the permissions of a file, simply use the \"chmod octal
		filename\" command where \"octal\" is the 3-digit octal number
		corresponding to what you want to change the file permissions to.  For
		instance, if you wanted to change a file's permission to
		\"rwxrw-r--\", you would use the \"chmod 764 filename\" command. ''')

	text_question(["chmod 644 apple"],
		     	  '''
				  How would you use the \"chmod\" command to change the permissions of
				  \"apple\" to \"rw-r--r--\"? ''',
				  "Hint: \"rw-r--r--\" corresponds to \"644\" in octal ")

	print("\nLet's check that that worked with the \"ls -l\" command.")

	print_command("ls -l")

	print("\nNice job, it worked! Next, you'll learn about how to change file ownership. ")

	end_of_section()

	# chown, chgrp
	os.system("clear")
	print_dedent('''
		Changing file ownership and group (chown, chgrp): 

		Finally, there are two commands \"chown\" and \"chgrp\" that allow you
		to change the file ownership and file group of a particular file or
		directory, respectively. To change the ownership of a file use the
		\"chown newuser filename\" command, where \"newowner\" is the username
		of the new owner. Likewise, to change the file group of a file, use
		the \"chgrp newgroup filename\" command where \"newgroup\" is some
		group that you belong to. ''')

	print("Nice job, you finished Principle 4! ")

	input("\nPress enter to close the tutorial.")

def comp5_sect1():
	os.system("clear")

	print_dedent('''
		Principle 5, Section 1: UNIX shells and shell variables

		In this section, you will learn about UNIX shells. You will also
		learn about shell variables and their uses. In particular you 
		will learn how to: 

		1) Open new shell processes and change your default shell
		2) Use several useful common bash variables
		3) Display, set, and unset the value of shell variables 
		4) Set the value of shell variables (advanced)
		5) Determine the type and location of a command
	''')

	end_of_section()

	###
	# sh, csh, bash, chsh
	###
	os.system("clear")

	print_dedent('''
		1) Open new shell processes and change your default shell (sh, csh, bash, bhsh):

		UNIX shells are the environments in where we run commands and programs. 
		In fact, you are using a shell right now! Often, shells are opened
		automatically when you start a program. For instance, if you're using a Mac, 
		you're probably using the Terminal program; alternatively, if you on a PC, 
		you might be using the Ubuntu app. Both of these programs will open up a shell. 
		There are also other ways to open up a shell, which we will go into shortly.

		There are many different types, or "flavors", of shells. The original shell 
		was called "sh", and a later, also popular, shell was called "csh". However,
		in this tutorial we will be talking about the "bash" shell, as it is the shell
		that is the most commonly used today (and it is installed by default on the Zoo).

		In addition to being the name of a shell, "bash" is also a UNIX command that 
		can be used to create a new bash process. When you enter the "bash" command, 
		a new bash process will be created inside your existing shell. Feel free to
		try this out on your own later. (Note that you can use the "exit" command to 
		close your current shell.)

		Finally, the "chsh" command is a command that allows you to change your default
		shell. You probably won't be using it very much (if at all), but we introduce 
		you to the command in the case you do wish to change your default shell one day. ''')

	end_of_section()

	###
	# HOME, PWD, OLDPWD, RANDOM, SHELL, USER, UID
	###
	os.system("clear")

	print_dedent('''
		2) Use several useful common bash variables:

		Bash (and other shells) have several environment variables that help keep 
		track of useful information related to the user and their processes. In 
		this section, we will introduce you to several useful bash variables.

		For instance, HOME is a variable in bash that stores the location of your 
		your home directory. Let's say we wanted to print out this variable. We,
		could use the "echo" command to print out the value of HOME. However, we 
		need some way to distinguish between just the word HOME and the variable 
		HOME. For instance, note what happens when we just execute "echo HOME". ''')

	print_command("echo HOME")

	print_dedent('''
		Thus, in UNIX there is a special usage for the dollar sign $. If we precede
		the variable name with $, this allows us to get the value of that variable. 
		Check it out in the example below: ''')

	print_command("echo $HOME")

	print_dedent('''
		Some other useful variables are:
		PWD (the location of your current directory)
		OLDPWD (the location of your previous directory) 
		RANDOM (a random number between 0 and 32767)
		SHELL (your default shell) 
		USER (your NetID)
		UID (your numeric user id)''')

	text_question(["echo $PWD"],
		          '''
		          Let's try out using one of these variables. 

		          How would you use "echo" and the PWD variable to display your current 
		          directory?''',
		          "Hint: Type \"echo $PWD\" ")

	print_dedent('''
		Nice job! Now, you might be wondering what the different is between the 
		"echo $PWD" and "pwd" commands. In terms of output, these commands basically 
		do the same thing. In fact, many of the shell variables can be used to create 
		the same output as various UNIX commands. For instance, the \"echo $USER\" 
		command has the same output as the \"whoami\" command. ''')

	end_of_section()

	###
	# set, export, unset
	###
	os.system("clear")

	print_dedent('''
		3) Display, set, and unset the value of shell variables (set, export, unset):

		Now, you will learn how to display, set, and unset the values of your own shell
		variables. 

		First, we will introduce you to the "set" command. The "set" command (with no 
		arguments provided) simply prints out all of the shell variables and shell 
		functions. There is a lot of output, so let's just take a look at the first 
		10 lines below by using the "head" command.''')

	print_command("set | head")

	print_dedent('''
		As you can see above, "set" simply displays the value of shell variables. 
		This might be somewhat confusing, as the command is named "set". 
		(Note: the "set" command can be used to set the value of some specific 
		shell variables, but that is outside of the scope of this tutorial.)

		Instead, there are two main ways to set the values of variables in bash. 
		First, you can just type "variable=value" in the terminal, where "variable" 
		is the name of the variable you are setting and "value" is the value of that 
		variable. Alternatively, you can enter "export variable=value" into the 
		terminal. It is important in both cases that you do not have any spaces next 
		to the "=".

		(There is a small difference between the two commands: using "export" will 
		allow future subprocesses to access the variables. Don't worry about this 
		nuance too much though.)''')

	text_question(["HELLO=world", "HELLO=\"world\"", "export HELLO=world", "export HELLO=\"world\""],
		          '''
		          Let's try setting a variable. 

		          How would you use set the value of a variable HELLO to be equal to 
		          "world"?''',
		          "Hint: Type \"HELLO=world\". Don't include any spaces! ")

	# Need to manually set variable; not worth defining own question type
	os.environ["HELLO"] = "world"

	text_question(["echo $HELLO"],
		          '''
		          Nice! 

		          Do you remember how to print out the value of a variable? 
		          Let's try printing out the value of HELLO. ''',
		          "Hint: Type \"echo $HELLO\". ")

	print_dedent('''
		Nice job! Finally we'll learn how to unset variables. To unset a bash 
		variable, just use the "unset variable" command. Let's try this out 
		below! ''')

	text_question(["unset HELLO"],
		          '''
		          How would you unset the HELLO variable? ''',
		          "Hint: Type \"unset HELLO\". ")

	del os.environ['HELLO']

	print_dedent('''
		Great! Let's check that worked!''')

	print_command("echo $HELLO")

	print_dedent('''
		Nice job! As you can see, the "echo $HELLO" command above just printed out an
		empty line. ''')

	end_of_section()

	###
	# 
	###
	os.system("clear")

	print_dedent('''
		4) Set the value of shell variables (advanced)

		In this section, we'll talk about different kinds of syntax you should be 
		aware of when setting the value of shell variables. 

		First of all, you can set the value of a variable equal to the result of a 
		UNIX command by using the backticks, `, or dollar sign with a single set of
		parentheses, $(). Consider the following examples:''')

	# set and print value of A
	print_command("A=`whoami`")
	os.environ["A"] = os.popen("whoami").read()
	print_command("echo $A")

	# set and print value of B
	print_command("B=$(whoami)")
	os.environ["B"] = os.popen("whoami").read()
	print_command("echo $B")

	# set and print value of C
	print_command("C=whoami")
	os.environ["C"] = "whoami"
	print_command("echo $C")

	text_question(["DIR=`pwd`", "DIR=$(pwd)"],
		          '''
		          As you can see, the value for "whoami" was stored in "A" and "B". 
		          However, because we didn't use the backticks or $() for "C", the value
		          that was stored was the string "whoami" instead of the value of the 
		          output of the command. 

		          Now, let's try this out ourselves. How would you set the value of 
		          the variable DIR equal to the results of the "pwd" command?''',
		          "Hint: Type DIR=`pwd`")

	print_dedent('''
		Cool! Let's print this out to check! ''')

	# set and print value of DIR
	os.environ["DIR"] = os.popen("pwd").read()
	print_command("echo $DIR")

	print_dedent('''
		Nice job! In addition, you can use the "expr" command to evaluate mathematical 
		expressions in UNIX. For instance, see the example below:''')

	print_command("expr 5 + 2")

	print_dedent('''
		As you can see, that above command evaluated "5 + 2" and printed out "7". 
		Let's try this out below!''')

	text_question(["SUM=`expr 5 + 2`", "SUM=$(expr 5 + 2)"],
		          '''
		          How would you set the variable SUM equal to the expression "5 + 2"
		          by using "expr" command? Remember that you can use backticks to set 
		          a variable equal to the result of a command.''',
		          "Hint: Type \"SUM=`expr 5 + 2`\"")

	print_dedent('''
		Nice job! Let's print out the value to check''')

	# set and print value of SUM
	os.environ["SUM"] = "7"
	print_command("echo $SUM")

	print_dedent('''
		Cool! Finally, we will introduce you to an alternative to using "expr" 
		with backticks. You can set a variable equal to an expression if you use 
		a dollar sign with two sets of parentheses, $(()), or one set of square 
		brackets, $[]. Take a look below!''')

	# set and print value of E
	print_command("D=$((1 + 3))")
	os.environ["D"] = "4"
	print_command("echo $D")

	# set and print value of F
	print_command("E=$[2 + 3]")
	os.environ["E"] = "5"
	print_command("echo $E")

	print_dedent('''
		As you can see, there are lots of different ways to set the values of 
		variables in bash. ''')

	end_of_section()

	###
	# PATH, which, type
	###
	os.system("clear")

	print_dedent('''
		5) Determine the type and location of a command (type, which, PATH):

		There are different kinds of bash commands. Some commands are built-in to bash. 
		Some examples are "cd" and "echo". One advantage of built-in commands are that 
		they are a bit faster to execute. 

		On the other hand, some commands are actually executable files that are stored 
		somewhere in the computer. An example is the "cat" command. These commands take 
		a bit longer to execute.

		One way to determine if a command is a built-in is to use the "type command"
		command. For instance, let's take a look at the following examples.''')

	print_command("type cd")

	print_command("type cat")

	print_dedent('''
		As you can see from the above output, "cd" is a shell built-in. On the other 
		hand, because it says "cat is /usr/bin/cat", you can tell that "cat" is not a 
		shell built-in. Rather, that file "/usr/bin/cat" is the file that is executed 
		when you run the "cat" command. 

		You can also use the "which command" command to figure out where a particular 
		command is. Let's try this out below!''')

	text_question(["which cat"],
		          '''
		          How would you use "which" to find the location of the "cat" command?''',
		          "Hint: Type \"which cat\"")

	print_dedent('''
		Nice job! The "which" command actually only searches through a specific list of 
		directories when it is looking for the given command. This list is stored in the 
		PATH variable. Let's take a look at the PATH variable.''')

	print_command("echo $PATH")

	print_dedent('''
		The above is the list of directories, separated by a ":", that "which" searches 
		through. 

		In case you are wondering, when you type a UNIX command, there is a process 
		that UNIX goes through to determine what to execute. First, UNIX checks to see 
		if it is a built-in UNIX command (e.g. "cd"). Next, UNIX checks to see if you 
		have typed in the path of an executable (e.g. "/usr/bin/cat"). Finally, bash 
		searches through the PATH, and sees if it can find the command there. If it 
		cannot, then bash gives you an error message.

		Nice job! You've finished this section of Principle 5. ''')

	end_of_section()

def comp5_sect2():
	os.chdir(os.path.expanduser("~/tutorial/comp5"))
	os.system("clear")

	print_dedent('''
		Principle 5, Section 2: Helpful UNIX commands

		In this section, you will learn about some helpful UNIX commands. 

		For instance, you'll learn how to:
		1) Package and unpackage tar archives
		2) Compress and uncompress files
		3) Print out your UNIX history
		4) Execute past commands with shortcuts
		5) Source a script file
		6) Search for commands
		7) Find files 
	''')

	end_of_section()

	###
	# tar
	###
	os.system("clear")

	print_dedent('''
		1) Package and unpackage tar archives (tar):

		You can combine multiple files into a tar archive using the "tar" command. 
		In our directory, we have "hello" and "bye" files. We can combine them into 
		a file called "file.tar" by using the "-cf" option:''')

	print_command("tar -cf file.tar hello bye")

	print_command("ls")

	print_dedent('''	
		As you can see, we've made a new tar archive called "file.tar". Let's now 
		delete our "hello" and "bye" files.''')

	print_command("rm hello bye")

	print_command("ls")

	print_dedent('''	
		We can now unpack that tar archive to get our files back by using the
		"-xf" option.''')

	print_command("tar -xf file.tar hello bye")

	print_command("ls")

	print_dedent('''
		This was a short example on how you would use the "tar" command. Feel free 
		to look into the command if you're interested in learning about other ways 
		you can use the command.  ''')

	end_of_section()

	###
	# gzip, gunzip
	###
	os.system("clear")

	print_dedent('''
		2) Compress and uncompress files (gzip, gunzip):

		You can compress a file with the "gzip" command and uncompress a file
		using the "gunzip" command. For example, we would compress the "hello" 
		file with the following command:''')

	print_command("gzip hello")

	print_command("ls")

	text_question(["gunzip hello.gz"],
		          '''
		          As you can see, this replaced the "hello" file with a compressed file
		          called "hello.gz". Now let's try unzipping it ourselves. 

		          How would you use "gunzip" to uncompress the "hello.gz" file? ''',
		          "Hint: Type \"gunzip hello.gz\" ")

	print_dedent('''	
		Nice! Let's check to see if that worked.''')

	print_command("ls")

	print_dedent('''	
		Nice job! Now you know how to compress and uncompress files. ''')

	end_of_section()

	###
	# history
	###
	os.system("clear")

	print_dedent('''
		3) Print out your UNIX history (history)

		You can print out your UNIX history using the "history" command. By default,
		bash stores the last 500 commands that you executed. Accordingly, the 
		"history" command prints out the last 500 UNIX commands that you entered 
		in the terminal.

		You can also use the "history x" command where "x" is a number to print out 
		the last "x" commands in the history. Let's try this out below!''')

	text_question(["history 10"],
		          '''
		          How would you print out the last 10 commands you used in UNIX?''',
		          "Hint: Type \"history 10\" ")

	print_dedent('''
		Nice job! You can also use the "history" command with the "grep" command 
		we learned about earlier. For instance, if you wanted to see the last times 
		that you used the "ls" command, you could use the "history | grep ls" 
		command.''')

	end_of_section()

	###
	# !
	###
	os.system("clear")

	print_dedent('''
		4) Execute past commands with shortcuts

		There are some really helpful UNIX shortcuts to execute past commands. 
		For instance, the "!!" shortcut executes the previous command you 
		executed. Similarly the "!-x" shortcut, where "x" is some number, 
		executes the command you executed "x" commands ago. For example, 
		the "!-3" command executes the third previous command. ''')

	plaintext_question(["!-2"],
					   '''
					   How would you execute your second previous command? ''',
					   "Hint: type \"!-2\"")

	print_dedent('''
		Nice job!

		In addition the "!com" shortcut, where "com" is some UNIX command, 
		executes your most recent call of the "com" command. For instance, 
		the "!ls" command would execute your most recent "ls" command. ''')

	plaintext_question(["!cat"],
					   '''
					   How would you execute your most recent "cat" command? ''',
					   "Hint: type \"!cat\"")

	print_dedent('''
		Good job! You can also carry out a specific command from your UNIX 
		history. You might remember that the "history" command prints out a 
		number for each command in the history output. If you wanted to
		carry out the command numbered "x" in that history, you would just
		use the "!x" command. ''')

	end_of_section()

	###
	# source
	###
	os.system("clear")

	print_dedent('''
		5) Source a script file (source):

		The "source filename" command allows you to execute lines in the file 
		"filename" as if you had typed them into the terminal. For instance,
		take a look at the file "hello":''')

	print_command("cat hello")

	print_dedent('''
		As you can see from the above output, "hello" contains the line 
		"echo hello world!". This means that if we run the "source hello" 
		command, we expect the output to be the results of "echo hello world".
		Let's see if this works:''')

	print_command("source ./hello")

	print_dedent('''
		Nice! It worked! ''')

	end_of_section()

	###
	# source
	###
	os.system("clear")

	print_dedent('''
		5) Source a script file (source):

		The "source filename" command allows you to execute lines in the file 
		"filename" as if you had typed them into the terminal. For instance,
		take a look at the file "hello":''')

	print_command("cat hello")

	print_dedent('''
		As you can see from the above output, "hello" contains the line 
		"echo hello world". This means that if we run the "source hello" 
		command, we expect the output to be the results of "echo hello world".
		Let's see if this works:''')

	print_command("source ./hello")

	print_dedent('''
		Nice! It worked! ''')

	end_of_section()

	###
	# apropos
	###
	os.system("clear")

	print_dedent('''
		6) Search for commands (apropos):

		You can use the "apropos" command to search for commands that are 
		similar to a given string. For instance, if you wanted to 
		see all the commands that are related to "python", you would
		type "apropos python". Let's try this out ourselves: ''')

	text_question(["apropos racket"],
		          '''
		          How would you print out the commands that are related
		          to "racket" using the "apropos" command?''',
		          "Hint: Type \"apropos racket\" ")

	print_dedent('''
		Nice job! ''')

	end_of_section()

	###
	# find
	###
	os.system("clear")

	print_dedent('''
		7) Find files (find):

		You can use the "find" command to search for files in a particular
		directory (and that directory's children). The "find" command has a ton 
		of different options that you should look into if you are interested.

		For example, if you wanted to print out all the files whose filenames 
		contained "hello" in your current directory (denoted by ".") and its 
		children, you could use following command: ''')

	print_command("find -name \"hello\" -print")

	print_dedent('''
		This is just one example of many. Feel free to look into this command! ''')

	end_of_section()

def comp5():
	os.system("clear")
	os.chdir(os.path.expanduser("~/tutorial/comp5"))

	print_dedent('''
		Welcome to Principle 5 of our UNIX tutorial!

		In this Principle, you're going to learn about shells as well
		as some other helpful UNIX commands. 
		Let's get started!''')

	end_of_section()

	comp5_sect1()
	comp5_sect2()

def comp6():
	os.system("clear")
	os.chdir(os.path.expanduser("~/tutorial/"))

	print_dedent('''
		Welcome to Principle 6 of our UNIX tutorial!

		In this Principle, you're going to learn a few UNIX commands related 
		to processes and process control.

		Let's get started!''')

	end_of_section()

	###
	# ps, sleep, &, ^Z
	###
	os.system("clear")
	print_dedent('''
		Introduction to processes:

		Every time you run a command in UNIX that is not a shell built-in, 
		UNIX creates a new process to execute that command. 

		You can use the "ps" command to see what processes are currently running.
		Let's take a look!''')

	print_command("ps")

	print_dedent('''
		From the above output, you probably see that there are at least 3 
		processes running. The "bash" process that is running is the bash shell
		that you are using in your terminal. The "python3" process shows that 
		python is being run (because this tutorial was written in python).
		Finally, the "ps" process is being run, because you are using "ps" to 
		see what processes are running. ''')

	print_dedent('''
		In addition, we'll introduce you to the "sleep x" command, where "x" is 
		a number. The sleep command simply does nothing for "x" seconds. We 
		introduce you to this command because it will be a helpful command to use 
		as an example during this section of the tutorial.''')

	text_question(["sleep 5"],
		          '''
		          How would you use the "sleep" command to wait for 5 seconds? ''',
		          "Hint: Type \"sleep 5\" ")

	print_dedent('''
		Nice job! Processes by default are run in the foreground. This is why the
		"sleep 5" process stops us from doing anything for 5 seconds. In the future, 
		if you don't want to wait for a foreground process to finish, you can use 
		CTRL+Z to suspend any process running in the foreground.
		
		In addition, you can also run processes in the background. You can do
		this by typing "&" after a command to run a command in the background. For 
		instance, we could run "sleep 5" in the background with the "sleep 5 &" 
		command.''')

	end_of_section()

	###
	# jobs, kill, bg, fg
	###
	os.system("clear")
	print_dedent('''
		Introduction to jobs:

		Jobs are processes started by the shell. You can take a look at what jobs 
		are currently running using the "jobs" command. Let's take a look!''')

	print_command("jobs")

	print_dedent('''
		As you can see, we don't currently have any jobs running. Let's change 
		that! We can run a sleep command in the background, and then check our 
		jobs. Take a look below! ''')

	# have to kind of do some hacky stuff to display the right information 
	# to students because of difficulty in handling child subshells
	print_dedent('''
		-bash-4.4$ sleep 100 &

		-bash-4.4$ jobs''')
	subprocess.check_call(r"""sleep 100 &
	jobs""", shell=True)

	print_dedent('''
		As you can see above, there is a job running for the "sleep" command
		we just ran. You might also notice that the row for that command is
		numbered "[1]". This means that we can refer to this job as job number
		1. Now that we know the job number, there are a few things we can do 
		with this job. 

		First of all, if you wanted to kill this job, you would use the 
		"kill %1" command. This will stop the process. Alternatively, you 
		could move the job from the background to the foreground, you would 
		use the "fg %1". Likewise, you could move the job from the foreground 
		to the background with the "bg %1" command (this wouldn't make much 
		sense in this particular case because "sleep 5 &" is already running 
		in the background). Of course, if you wanted to call any of these 
		commands with a job with a different job number, you would replace 
		the "1" with the number of your specific job. 

		We strongly encourage you to play with these commands yourself in the 
		terminal once you have finished this tutorial. ''')

	end_of_section()

	###
	# top
	###
	os.system("clear")
	print_dedent('''
		Viewing all processes with a GUI (top):

		Finally, if you're interested in a command that lets you see even 
		more information about the processes that are running on your computer,
		you can use the "top" command. This command will open a GUI, of all the 
		different processes running on your computer. You can click the "q" key 
		to exit the GUI.

		Congratulations, you've finished the tutorial!''')

	end_of_section()

# https://stackoverflow.com/questions/1265665/how-can-i-check-if-a-string-represents-an-int-without-using-try-except/1265696
def RepresentsInt(s):
    try: 
        int(s)
        return True
    except ValueError:
        return False

def main():
	# Boot-up stuff
	os.chdir(os.environ['HOME'])

	# Create folders and stuff here
	os.system("mkdir tutorial")
	os.system("echo 'Hello, world!' >  tutorial/apple")
	os.system("chmod 764 tutorial/apple")
	os.system("echo 'This is a test file!' >  tutorial/test")
	os.system("chmod 744 tutorial/test")

	# C1_S3
	os.system("mkdir tutorial/comp1")
	os.system("echo 'To buy: \nDozen eggs \n1 gallon milk \n3 potatoes' > tutorial/comp1/shopping_list")
	os.system("echo 'Blue and white are my favorite colors.' > tutorial/comp1/favorite_colors")
	os.system("echo 'I like raspberries and mangoes.' > tutorial/comp1/favorite_fruit")
	os.system("echo 'This is line 0 of a very long file. \nThis is line 1 of a very long file. \nThis is line 2 of a very long file. \nThis is line 3 of a very long file. \nThis is line 4 of a very long file. \nThis is line 5 of a very long file. \nThis is line 6 of a very long file. \nThis is line 7 of a very long file. \nThis is line 8 of a very long file. \nThis is line 9 of a very long file. \nThis is line 10 of a very long file. \nThis is line 11 of a very long file. \nThis is line 12 of a very long file. \nThis is line 13 of a very long file. \nThis is line 14 of a very long file. \nThis is line 15 of a very long file. \nThis is line 16 of a very long file. \nThis is line 17 of a very long file. \nThis is line 18 of a very long file. \nThis is line 19 of a very long file. \nThis is line 20 of a very long file. \nThis is line 21 of a very long file. \nThis is line 22 of a very long file. \nThis is line 23 of a very long file. \nThis is line 24 of a very long file. \nThis is line 25 of a very long file. \nThis is line 26 of a very long file. \nThis is line 27 of a very long file. \nThis is line 28 of a very long file. \nThis is line 29 of a very long file. \nThis is line 30 of a very long file. \nThis is line 31 of a very long file. \nThis is line 32 of a very long file. \nThis is line 33 of a very long file. \nThis is line 34 of a very long file. \nThis is line 35 of a very long file. \nThis is line 36 of a very long file. \nThis is line 37 of a very long file. \nThis is line 38 of a very long file. \nThis is line 39 of a very long file. \nThis is line 40 of a very long file. \nThis is line 41 of a very long file. \nThis is line 42 of a very long file. \nThis is line 43 of a very long file. \nThis is line 44 of a very long file. \nThis is line 45 of a very long file. \nThis is line 46 of a very long file. \nThis is line 47 of a very long file. \nThis is line 48 of a very long file. \nThis is line 49 of a very long file. \nThis is line 50 of a very long file. \nThis is line 51 of a very long file. \nThis is line 52 of a very long file. \nThis is line 53 of a very long file. \nThis is line 54 of a very long file. \nThis is line 55 of a very long file. \nThis is line 56 of a very long file. \nThis is line 57 of a very long file. \nThis is line 58 of a very long file. \nThis is line 59 of a very long file. \nThis is line 60 of a very long file. \nThis is line 61 of a very long file. \nThis is line 62 of a very long file. \nThis is line 63 of a very long file. \nThis is line 64 of a very long file. \nThis is line 65 of a very long file. \nThis is line 66 of a very long file. \nThis is line 67 of a very long file. \nThis is line 68 of a very long file. \nThis is line 69 of a very long file. \nThis is line 70 of a very long file. \nThis is line 71 of a very long file. \nThis is line 72 of a very long file. \nThis is line 73 of a very long file. \nThis is line 74 of a very long file. \nThis is line 75 of a very long file. \nThis is line 76 of a very long file. \nThis is line 77 of a very long file. \nThis is line 78 of a very long file. \nThis is line 79 of a very long file. \nThis is line 80 of a very long file. \nThis is line 81 of a very long file. \nThis is line 82 of a very long file. \nThis is line 83 of a very long file. \nThis is line 84 of a very long file. \nThis is line 85 of a very long file. \nThis is line 86 of a very long file. \nThis is line 87 of a very long file. \nThis is line 88 of a very long file. \nThis is line 89 of a very long file. \nThis is line 90 of a very long file. \nThis is line 91 of a very long file. \nThis is line 92 of a very long file. \nThis is line 93 of a very long file. \nThis is line 94 of a very long file. \nThis is line 95 of a very long file. \nThis is line 96 of a very long file. \nThis is line 97 of a very long file. \nThis is line 98 of a very long file. \nThis is line 99 of a very long file.' > tutorial/comp1/long_textfile")

	# C2_S2
	os.system("mkdir tutorial/comp2")
	os.system("touch tutorial/comp2/file1.txt")
	os.system("touch tutorial/comp2/file2.txt")
	os.system("touch tutorial/comp2/file3.txt")
	os.system("touch tutorial/comp2/file4.txt")
	os.system("touch tutorial/comp2/file5.txt")
	os.system("touch tutorial/comp2/file6.txt")

	os.system("touch tutorial/comp2/hw.rkt")
	os.system("touch tutorial/comp2/hw1.rkt")
	os.system("touch tutorial/comp2/hw11.rkt")
	os.system("touch tutorial/comp2/hw111.rkt")

	os.system("touch tutorial/comp2/a.pdf")
	os.system("touch tutorial/comp2/asdf.pdf")
	os.system("touch tutorial/comp2/dfdafs.pdf")
	os.system("touch tutorial/comp2/asdfasd.pdf")

	os.system("touch tutorial/comp2/assadfdf")
	os.system("touch tutorial/comp2/asdfdaasdf")
	os.system("touch tutorial/comp2/fdsasdfasdf")
	os.system("touch tutorial/comp2/dsdasfasdf")

	# C3_S1
	os.system("mkdir tutorial/comp3")
	os.system("echo 'Hello, world! \nGoodbye! \nfoo ' > tutorial/comp3/file1.txt")
	os.system("echo 'Hello, world! \nbar \nGoodbye! ' > tutorial/comp3/file2.txt")

	os.system("echo '#!/usr/bin/env python3' > tutorial/comp3/script.py")
	os.system("echo '#!/bin/bash' > tutorial/comp3/script2.sh")

	# C5_S2
	os.system("mkdir tutorial/comp5")
	os.system("echo 'echo hello world!' > tutorial/comp5/hello")
	os.system("echo 'echo bye bye!' tutorial/comp5/bye")

	# Tutorial beginning
	os.system("clear")
	print_dedent('''
		Welcome to our UNIX tutorial! 

		Our tutorial is made out of several different Principles.  These
		Principles correspond to the Principles in Dr. Slade's Linux
		introduction.

		There are six Principles in this tutorial.

		1. The file system is a tree.
		2. input/output is a character stream.
		3. There are many ways to find out stuff about UNIX.
		4. Security exists at the owner, group, and world level.
		5. UNIX has a robust command processor and associated environment variables. 
		6. UNIX provides support for process control.
		''')

	user_input = input("Please enter which number Principle you wish to learn about [1-6]: ")

	while not RepresentsInt(user_input) or int(user_input) not in range(1,7):
		user_input = input("Please enter a number between 1 and 6: ")

	comp = int(user_input)

	if comp == 1:
		comp1()
	elif comp == 2:
		comp2()
	elif comp == 3:
		comp3()
	elif comp == 4:
		comp4()
	elif comp == 5:
		comp5()
	elif comp == 6:
		comp6()
	else:
		pass


if __name__ == '__main__':
	main()