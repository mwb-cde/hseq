#!/usr/bin/env python3
######################################################################
# Copyright (c) 2017-2021 Matthew Wahab <mwb.cde@gmail.com>
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.
######################################################################

###
# Imports

import sys
import os.path
import glob
import fnmatch
import re
import shlex
import shutil
import subprocess
import argparse
import configparser
import typing

###
# Error handling

def program():
    '''Get the program name.'''
    return "testdriver.py"
#    return os.path.basename(sys.argv[0])

def error(msg, out = sys.stderr):
    '''Handle fatal errors.'''
    out.write(program() + ':error: ' + msg);
    out.flush()
    exit(-1)

def warning(msg, out = sys.stderr):
    '''Handle non-fatal problems.'''
    out.write(program() + ':warning: ' + msg);
    out.flush()

def info(msg, ok = True, out = sys.stderr):
    '''Print information.'''
    if ok:
        out.write(program() + ':info: ' + msg);
        out.flush()

def report(msg, ok = True, out = sys.stderr):
    '''Print quiet information.'''
    if ok:
        out.write(msg);
        out.flush()

###
# Test file and configurations.

class TestFile:
    '''A test file and its configuration.'''

    def split_string(self, value: str, sep: str = ',') -> list:
        '''Split a string into a list of strings. '''
        return [ x.strip() for x in value.split(sep) if x != '' ]


    def setup(self, values):
        '''Set up the configuration from the key-value map [values].

        Known keys:

        - test-type (field: test_type): The list of testers to use for this test
          or 'ignore' to mark a file as not a test.

        - sources: Extra source files to be compiled to build the test
          program.

        - libs: Extra libraries to be linked in. (Must include any required
          flags.)

        - compiler: The command to use compile a source file to an object
          file.

        - compiler_flags: The flags to pass to the compiler instead of the
          compiler flags specified by the tester.

        - extra_compiler_flags: Compiler flags to be added to the normal
          compiler flags.

        - linker: The command to use link a set of object files into an
          executable program.

        - linker_flags: The flags to pass to the linker instead of the
          linker flags specified by the tester.

        - extra_linker_flags: Linker flags to be added to the normal
          linker flags.

        '''

        test_type = values.get('test-type', None)
        if test_type != None:
            self.test_type = self.split_string(test_type, ',')

        sources = values.get('sources', None)
        if sources != None:
            self.sources = self.split_string(sources, ',')
        else:
            self.sources = []

        libs = values.get('libs', None)
        if libs != None:
            self.libs = self.split_string(libs, ',')
        else:
            self.libs = []

        compiler = values.get('compiler', None)
        if compiler != None:
            self.compiler = self.split_string(compiler, ' ')

        compiler_flags = values.get('compiler-flags', None)
        if compiler_flags != None:
            self.compiler_flags = self.split_string(compiler_flags, ' ')

        extra_compiler_flags = values.get('extra-compiler-flags', None)
        if extra_compiler_flags != None:
            self.extra_compiler_flags = (self.split_string
                                         (extra_compiler_flags, ' '))

        linker = values.get('linker', None)
        if linker != None:
            self.linker = self.split_string(linker, ' ')

        linker_flags = values.get('linker-flags', None)
        if linker_flags != None:
            self.linker_flags = self.split_string(linker_flags, ' ')

        extra_linker_flags = values.get('extra-linker-flags', None)
        if extra_linker_flags != None:
            self.extra_linker_flags = self.split_string(extra_linker_flags,
                                                        ' ')
        libs = values.get('libs', None)
        if libs != None:
            self.libs = self.split_string(libs, ' ')

    def __init__(self, name: str, values = {}):
        '''Initialise a testfile record.'''
        # The file name
        self.filename = name
        # The uninterpreted configuration key-value map.
        self.config_values = values
        # The test type or 'ignore'. Used to select a tester.
        self.test_type = None
        # sources: Extra source files.
        self.sources = None
        # libs: Extra libraries
        self.libs = None
        # Compiler command.
        self.compiler = None
        # Compiler flags.
        self.compiler_flags = None
        # Extra compiler flags.
        self.extra_compiler_flags = None
        # Linker command.
        self.linker = None
        # Linker flags.
        self.linker_flags = None
        # Extra linker flags.
        self.extra_linker_flags = None
        # Extra libraries (with any required flags)
        self.libs = []

        # Setup from a key-values map.
        if values != None:
            self.setup(values)
        else:
            self.setup({})

###
# Options and configurations

class Options:
    '''Collect the program options and configuration values in one place.'''

    def __init__(self, name, args = None, config = None):
        '''Initialise from a command line  and configuration record.'''
        self.name = name
        self.args = args
        self.config = config


    def verbosity(self):
        if self.args != None:
            return self.args.verbose
        return 0

    def keep_temps(self):
        if self.args != None:
            return self.args.keep_temps
        return False

    def files(self):
        if self.args != None:
            return self.args.files
        return []

###
# Tester configurations.

def config_lookup(config, section: str, key, fallback = None):
    '''Look up the value for a section key.

    Returns the value for [key] in [section]. If [section] doesn't define [key],
    follows the inheritance chain until the key is found. Returns [fallback] if
    the key isn't defined.

    '''

    if config == None:
        return fallback

    if not (section in config):
        return fallback

    scn = config[section]

    value = None
    if key in scn:
        value = config.get(section, key)
        if value != '':
            return value
        else:
            fallback = value

    # Key defined not in section. Follow the inheritance chain.
    default_tester = Tester.default_tester_name()

    if 'inherit' in section:
        next_section = section['inherit']
    elif section != default_tester:
       next_section = default_tester
    else:
        return fallback

    return config_lookup(config, next_section, key, fallback)

class Tester:
    '''Tester for a class of language'''

    def default_tester_name():
        '''The name of the default tester.'''
        return 'default'

    def default_tester_config():
        '''Default configuration fields and values for a tester.'''
        return {
            # Which section to inherit undefined settings from.
            'inherit': '',
            # The suffixes for source files tested with the section. (A
            # comma seperated list.)
            'file-suffix': '',
            # The file recogniser for source files tested with the
            # section.
            'files': '*',
            # The suffix for object code files.
            'object-suffix': '.o',
            # The suffix for executable files..
            'executable-suffix': '.exe',
            # How to compile to an object file.
            'compiler': '',
            # Extra compiler flags.
            'compiler-flags': '',
            # How to link to build an executable.
            'linker': '',
            # Extra linker flags.
            'linker-flags': '',
            # libs: Extra libraries to be linked in. (Must include any required
            # flags.)
            'libs' : '',
            # Shell command to run the executable.
            'runner': '',
            # Flags to pass to the runner.
            'runner-flags': '',
        }

    def default_config(self):
        return {}

    class Config:
        '''Configuration values for use by a tester.

        Normally initialized from a section in a configuration file.'''

        def setup(self, name, config):
            '''Initialize a Tester.Config object from a config object.

            [name] is the name of the tester. This is used to identify testers
            and associate them with e.g. configurations.

            [config] should be a [configparser] object returned by configparser
            from which to read values to use.

            '''

            section = self.name

            suffixes_str = config_lookup(config, name, 'file-suffix')
            if suffixes_str == None or suffixes_str == '':
                warning("No value given for 'file-suffix' in section '"
                        + section + "'\n")
            else:
                self.suffixes = [ x.strip() for x in suffixes_str.split(',') ]

            self.files = config_lookup(config, name, 'files')
            if self.files == None or self.files == '':
                warning("No value given for 'files' in section '"
                        + section + "'\n")

            self.object_suffix = config_lookup(config, name, 'object-suffix')
            if (self.object_suffix == None
                or self.object_suffix == ''):
                warning("No value given for 'object-suffix' in section '"
                        + section + "'\n")

            self.executable_suffix = config_lookup(config, name,
                                                   'executable-suffix')
            if (self.executable_suffix == None
                or self.executable_suffix == ''):
                warning("No value given for 'executable-suffix' in section '"
                        + section + "'\n")

            compiler = config_lookup(config, name, 'compiler')
            if (compiler == None or compiler == ''):
                warning("No value given for 'compiler' in section '"
                        + section + "'\n")
            self.compiler = shlex.split(compiler)

            compiler_flags = config_lookup(config, name,
                                           'compiler-flags',
                                           fallback = '')
            self.compiler_flags = shlex.split(compiler_flags)

            linker = config_lookup(config, name, 'linker')
            if (linker == None or linker == ''):
                warning("No value given for 'linker' in section '"
                        + section + "'\n")
            self.linker = shlex.split(linker)

            linker_flags = config_lookup(config, name,
                                         'linker-flags',
                                         fallback = '')
            self.linker_flags = shlex.split(linker_flags)

            libs = config_lookup(config, name,
                                 'libs',
                                 fallback = '')
            self.libs = shlex.split(libs)

            runner = config_lookup(config, name, 'runner')
            if (runner == None):
                warning("No value given for 'runner' in section '"
                        + section + "'\n")
            self.runner = shlex.split(runner)

            runner_flags = config_lookup(config, name,
                                         'runner-flags',
                                         fallback = '')
            self.runner_flags = shlex.split(runner_flags)


        def __init__(self, name, desc = None, config = None):
            '''Initialize a Tester.Config object.

            [name] is the name of the tester. This is used to identify testers
            and associate them with e.g. configurations.

            If [config] is given, it should be a [configparser] object returned
            by configparser from which to read values to use.

            '''

            # Initialize (and specify) the fields.
            self.name = name  # The name of the test kind.
            self.invalid = True  # Whether the tester is valid.
            self.suffixes = []
            self.files = None
            self.object_suffix = None
            self.executable_suffix = None
            self.compiler = None
            self.compiler_flags = []
            self.linker = None
            self.linker_flags = []
            self.runner = None
            self.runner_flags = []

            if config != None:
                self.setup(config)


        def output(self, out = sys.stdout):
            '''Print the values of a test configuration object.'''
            out.write('Config(' + self.name + '){\n');
            out.write('invalid = ' + str(self.invalid) + '\n')
            out.write('file-suffix = '
                      + ','.join(self.suffixes)
                      + '\n')
            out.write('files = ' + str(self.files) + '\n')
            out.write('object-suffix = ' + str(self.object_suffix) + '\n')
            out.write('executable-suffix = '
                      + str(self.executable_suffix) + '\n')
            out.write('compiler = ' + str(self.compiler) + '\n')
            out.write('compiler-flags = ' + str(self.compiler_flags) + '\n')
            out.write('linker = ' + str(self.linker) + '\n')
            out.write('linker-flags = ' + str(self.linker_flags) + '\n')
            out.write('runner = ' + str(self.runner) + '\n')
            out.write('runner-flags = ' + str(self.runner_flags) + '\n')
            out.write('}\n')
            out.flush()


    ### Tester Methods.
    def default_config(self):
         '''Default configuration fields and values for a tester instance.'''
         Tester.default_tester_config()

    def __init__(self, name, desc = None, config = None):
        # Fields
        self.name = name
        self.desc = desc  # A description of the tester.
        self.invalid = True
        self.config = Tester.Config(name, config)

    def configure(self, config = None):
        '''Configure the tester.'''

        self.config.setup(self.name, config)

        # Check for a valid tester.
        self.invalid = False

        if self.config.suffixes == []:
            warning('Tester ' + self.name + ': No file suffix specifier.\n')
            self.invalid = True
        if self.config.files == None:
            warning('Tester ' + self.name + ': No files specifier.\n')
            self.invalid = True
        if self.config.object_suffix == None:
            warning('Tester ' + self.name + ': No object-suffix specifier.\n')
            self._invalid = True
        if self.config.compiler == None:
            warning('Tester ' + self.name + ': No compiler specified.\n')
            self._invalid = True
        if self.config.linker == None:
            warning('Tester ' + self.name + ': No linker specified.\n')
            self._invalid = True

    def report(self, out, msg, *args):
        '''Print information on an output stream.

        Prints on output stream [out].'''
        out.write(msg.format(args))
        out.flush()

    def warning(self, out, msg, *args):
        '''Print a warning on an output stream.

        Prints on output stream [out].'''
        out.write("warning: " + msg.format(args))
        out.flush()

    def make_unique_filename(self, filename: str, suffix: str) -> str:
        '''Make a unique version of [filename] that ends with [suffix].

        Returns the filename.
        '''
        unique_name = filename + suffix

        ctr = 1
        while os.path.exists(unique_name):
            unique_name = filename + '.' + str(ctr) + suffix
            ctr += 1

        return unique_name

    def make_logfile(self, filename, suffix = '.log'):
        '''Make a logfile for a filename.

        Returns the name of the log file and the file object.
        '''

        logfile = None
        logname = self.make_unique_filename(filename, '.log')

        try: logfile = open(logname, 'w')
        except:
            warning("Couldn't open log file " + logname + "\n")
            logname = None
            logfile = None

        return (logname, logfile)

    def make_object_name(self, filename: str):
        '''Make an object file name.'''
        object_name = self.make_unique_filename(filename,
                                                self.config.object_suffix)
        return object_name

    def make_executable_name(self, filename: str):
        '''Make an executable file name.'''
        program_name = self.make_unique_filename(filename,
                                                 self.config.executable_suffix)
        return program_name

    def accept(self, filename):
        '''Return whether a file is valid for this test.

        Only called if the tester has no file-suffixes defined.

        '''
        return False

    def get_sources(self, testfile: TestFile):
        '''Get the list of sources for a test file.'''
        assert(testfile != None)
        assert(testfile.filename != None)
        srcs = [testfile.filename] + testfile.sources
        return srcs

    def get_libs(self, testfile: TestFile):
        '''Get the list of libraries for a test file.'''
        assert(testfile != None)
        assert(testfile.filename != None)

        libs = testfile.libs
        if libs == None:
            libs = self.config.libs

        return libs

    def get_compiler(self, testfile: TestFile):
        '''Get the compiler and compiler flags for a testfile'''
        assert(testfile != None)

        compiler = testfile.compiler
        if compiler == None or compiler == []:
            compiler = self.config.compiler

        if testfile.compiler_flags != None:
            flags = testfile.compiler_flags
        else:
            flags = self.config.compiler_flags

        if testfile.extra_compiler_flags != None:
            flags += testfile.extra_compiler_flags

        return (compiler, flags)


    def extra_object_files(self, srcfile: str, objectfile: str):
        '''A list of the auxiliary files built alongside an object file.'''
        return []

    def compile_file(self, compiler, flags,
                     srcfile: str, wdir:str = None,
                     log = sys.stdout):
        '''Compile a file.

        Compiles file [srcfile] and any auxiliary files that it requires using
        [compiler] and compiler flags [flags] in directory [wdir]. Returns
        [(source, object, aux)] where [source] is the source file, [object] the
        built file and [aux] is True iff object is an auxiliary file. If
        [object] is [None] then the compilation failed for [source].

        '''

        objfile = self.make_object_name(srcfile)

        flags_str = ' '.join(flags)
        compiler_cmd =  [ x.format(object = objfile,
                                   flags = flags_str,
                                   source = srcfile)
                          for x in compiler ]
        cmd_line = [c for c in compiler_cmd
                    if not (len(c) == 0 or c.isspace())]
        cmd = [ shlex.quote(c) for c in cmd_line ]
        cmd_str = ' '.join(cmd)
        self.report(log, cmd_str + '\n')

        proc = subprocess.Popen(cmd_str, stdout = log, stderr = log,
                                shell = True, cwd = wdir)
        proc.wait()

        built = True
        if proc.returncode == None or proc.returncode != 0:
            self.warning (log, "Failed to compile " + srcfile + "\n")
            return [(srcfile, None, False)]

        return [(srcfile, objfile, False)]


    def compile(self, testfile: TestFile, wdir = None, log = sys.stdout):
        '''Compile a test file.

        Compiles each file needed for [testfile]. Returns a list of tuples
        [(source, object, built)] and a list of extra files. [built] is [True]
        iff [object] was successfully built form [source].

        '''

        if wdir == None:
            wdir = os.getcwd()

        (compiler_line, compiler_flags) = self.get_compiler(testfile)

        compiler = shutil.which(compiler_line[0])
        if compiler == None:
            self.warning(log, ('Invalid compiler specified: '
                               + compiler_line[0] + '\n'))
            return ([(testfile, '', False)], [])
        compiler_line[0] = compiler

        files = self.get_sources(testfile)
        objs = []
        extra = []

        for src in files:
            results = self.compile_file(compiler_line, compiler_flags,
                                        src, wdir, log)

            for (src, obj, aux) in results:
                built = True
                if obj == None:
                    built = False
                    self.warning (log, "Failed to compile " + src + "\n")

                if aux:
                    extra.append(obj)
                else:
                    objs.append((src, obj, built))

            aux_files = self.extra_object_files(src, obj)
            for x in aux_files:
                extra.append(x)

        # Done
        return (objs, extra)


    def get_linker(self, testfile: TestFile):
        '''Get the linker and linker flags for a testfile'''
        assert(testfile != None)

        linker = testfile.linker
        if linker == None or linker == []:
            linker = self.config.linker

        if testfile.linker_flags != None:
            flags = testfile.linker_flags
        else:
            flags = self.config.linker_flags

        if testfile.extra_linker_flags != None:
            flags += testfile.extra_linker_flags

        return (linker, flags)


    def extra_program_files(self, program: str):
        '''A list of the auxiliary files built alongside a program.'''
        return []


    def link(self, testfile: TestFile, objects, wdir = None,
             log = sys.stdout):
        '''Links the object files [objects] to make an executable called
        [program].

        Returns [(executable, aux_files, built)] where [program] is the
        executable, [built] is [True] iff [program] was successfully built and
        [aux_files] is the list of auxiliary files built alongside the program.

        '''

        linker, linker_flags = self.get_linker(testfile)
        linker_flags_str = ' '.join(linker_flags)

        objects_str = ' '.join(objects)
        program = self.make_executable_name(testfile.filename)

        libs = self.get_libs(testfile)
        libs_str = ' '.join(libs)

        cmd = [shutil.which(linker[0])]

        linker_cmd = [ x.format(program = program,
                                flags = linker_flags_str,
                                objects = objects_str,
                                libs = libs_str)
                       for x in linker[1:] ]

        cmd_line = [ c for c in linker_cmd
                     if not (len(c) == 0 or c.isspace()) ]
        cmd += [ shlex.quote(c) for c in cmd_line ]

        cmd_str = ' '.join(cmd)
        self.report(log, cmd_str + '\n')

        if wdir == None:
            wdir = os.getcwd()
        proc = subprocess.Popen(cmd_str, stdout = log, stderr = log,
                                shell = True, cwd = wdir)
        proc.wait()

        built = True
        aux_files = []
        if proc.returncode == None or proc.returncode != 0:
            built = False
            warning ("Failed to link " + program + " from "
                     + ", ".join(objects) + "\n")
        else:
            aux_files = self.extra_program_files(program)

        return (program, aux_files, built)

    def run(self, program, args, wdir = None, log = sys.stdout):
        '''Run [program] with arguments [args].

        Returns the return code for the program.'''

        runner = self.config.runner
        runner_flags = self.config.runner_flags

        if wdir == None:
            wdir = os.getcwd()

        cmd = runner
        cmd += runner_flags
        cmd = [shutil.which(program, path = '.')] + args
        self.report(log, ' '.join(cmd) + '\n')
        proc = subprocess.Popen(cmd, stdout = log, stderr = log, cwd = wdir)
        proc.wait()

        return proc.returncode

    class Status:
        '''The status of a test.

        One of PASS, FAILED or SKIP.'''

        UNDEF = 0
        SKIP = UNDEF + 1
        PASS = SKIP + 1
        FAIL = PASS + 1


    def test(self, testfile: TestFile, wdir = None):
        '''Test a file.

        Build and run the test in source file [testfile], recording the output
        in a newly created log, named after the file.

        Returns a tuple (status, testfile.filename, srcs, program, objs, extra)

        where
        - [status] is PASS, FAIL or SKIP depending on whether the test passed,
          failed or was skipped.
        - [testfile.filename] is the test file name passed in.
        - [srcs] is the full list of source files for the test.
        - [program] is the executable that was built and run.
        - [objs] is the list of built object files.
        - [extra] is the list of auxiliary files genrated as part of the build.

        '''

        assert(not self.invalid)
        assert(testfile != None)
        assert(testfile.filename != None)
        assert(testfile.filename != '')

        # Get the files that are known by this tester.
        assert(not (self.config.files == None
                    and self.config.suffixes == None))

        srcfile = testfile.filename
        passed = False
        failed = False
        skipped = False

        # Make a log file.
        (logfilename, logfile) = self.make_logfile(srcfile)
        if logfile == None:
            log = sys.stdout
        else:
            log = logfile
            assert(log != None)

        if wdir == None:
            wdir = os.getcwd()

        # Compile.
        (obj_list, extra) = self.compile(testfile, wdir, log)
        objs = []
        srcs = []
        for (src, obj, built) in obj_list:
            objs.append(obj)
            srcs.append(src)
            if not built:
                warning("Failed to build " + srcfile + "\n")
                failed = True

        # Link.
        program = None
        if not (failed or skipped):
            (program, aux_files, built) = self.link(testfile, objs, wdir, log)
            if not built:
                failed = True
            else:
                for x in aux_files:
                    extra.append(x)

        # Run.
        if not (failed or skipped):
            retcode = self.run(program, [], wdir, log)
            if retcode == 0:
                passed = True
            else:
                failed = True

        # Close the log file.
        if logfile != None:
            logfile.close()

        # Count.
        if passed:
            self.report(sys.stdout, srcfile + ": PASS\n")
            status = Tester.Status.PASS
        elif failed:
            self.report(sys.stdout, srcfile + ": FAIL\n")
            status = Tester.Status.FAIL
        elif skipped:
            self.report(sys.stdout, srcfile + ": SKIP\n")
            status = Tester.Status.SKIP
        else:
            warning('unknown test status for ' + srcfile + '\n')
            self.report(sys.stdout, srcfile + ": UNKNOWN\n")
            status = Tester.Status.SKIP

        # Return.
        return (status, srcfile, srcs, program, objs, extra)

###
# Test driver
class TestDriver:
    '''Run a set of tests'''

    def default_driver_name():
        '''Return the name for the test driver.'''
        return program()

    def default_driver_config():
        '''Return the default configuration for the driver.'''
        return {
            'directive-start' : '%%{',
            'directive-end' : '}%%'
        }

    def setup_directive_matcher(self, options: Options):
        '''Setup the regexp matcher for test directives.

        A test-directive line has the form:

        <comment-start><dir-start> <key><sep><value><dir-end><comment-end>

        with arbitrary amounts of whitespace (but not newlines) between the
        syntactic elements.
        '''
        def sep():
            '''The possible seperators between key-value pairs.

            Return a string containing a regexp to match the possible
            seperators for key-value pairs.

            '''
            return r"[:=]"


        def key():
            '''The possible forms for a key.

            Return a string containing a regexp to match the possible
            forms of a key.

            '''
            return r"[a-zA-Z0-9_\-]+"

        def value():
            '''The possible forms for a value.

            Return a string containing a regexp to match the possible
            forms of a value.

            '''
            return r".+"

        def directive_line(options: Options):
            '''The form of a directive line.

            Return a string containing a regexp to match a possible
            directive line.

            Assumes an arbitrary, possibly empty, string at the start and end of
            al line which is taken to be the comment start and end.

            '''

            dir_start_str = self.directive_start
            dir_end_str = self.directive_end

            if dir_start_str == None:
                warning("Directive start marker not specified.\n",
                        options.verbosity() > 0)
                return ""

            if dir_end_str == None:
                warning("Directive end marker not specified.\n",
                        options.verbosity() > 0)
                return ""

            whitespace_str = r"[ \t]*"
            any_str = r".*"

            dir_str_list = [r"^",
                            any_str,
                            r"(?P<dir_start>", dir_start_str, r")",
                            whitespace_str,
                            r"(?P<key>", key(), r")",
                            whitespace_str,
                            sep(),
                            whitespace_str,
                            r"(?P<value>", value(), r")",
                            whitespace_str,
                            r"(?P<dir_end>", dir_end_str, r")",
                            any_str,
                            r"$"]

            dir_str = ''.join(dir_str_list)
            return dir_str

        # setup_directive_matcher
        directive_str = directive_line(options)
        info("Test directive matcher:\n{" + directive_str + "}\n",
             options.verbosity() > 1)
        self.directive_re = re.compile(directive_str)

    def setup(self, options: Options, testers = []):
        '''Setup the test driver.'''

        # The directive markers and matcher.
        self.directive_start = config_lookup(options.config,
                                             TestDriver.default_driver_name(),
                                             "directive-start",
                                             fallback = None)
        self.directive_end = config_lookup(options.config,
                                           TestDriver.default_driver_name(),
                                           "directive-end",
                                           fallback = None)

        self.setup_directive_matcher(options)

        # The list of tests.
        self.testers = testers
        if options.config != None:
            for tester in self.testers:
                tester.configure(options.config)

        # Set up the tables of testers indexed by name and by file-suffix.
        for tester in self.testers:
            self.named_tester_table[tester.name] = tester

            if (tester.config.suffixes == None
                or tester.config.suffixes == []):
                self.other_testers.append(tester)
                continue

            for suffix in tester.config.suffixes:
                if suffix in self.tester_table:
                    self.tester_table[suffix].append(tester)
                else:
                    self.tester_table[suffix] = [tester]


    def __init__(self, options, testers = []):
        '''Initialise a test driver.'''

        # testers: The list of tests.
        self.testers = []

        # A dictionary of testers with the file suffix as key.
        self.tester_table = {}

        # A dictionary of testers with the name as key.
        self.named_tester_table = {}

        # A list of testers with no file-suffix specified.  These should define
        # a method [accept(file)] to test whether they are run on [file].
        self.other_testers = []

        # The directive start and end markers.
        self.directive_start = None
        self.directive_end = None

        # The directive regexp
        self.directive_re = None

        # Set up with given values.
        self.setup(options, testers)


    def process_directive_key(self, options: Options, key):
        '''Process a key read from a directive line.

        Just removes leading and trailing spaces.'''
        return key.strip().lower()

    def process_directive_value(self, options: Options, value):
        '''Process a value read from a directive line.

        Removes leading and trailing spaces and unquotes the resulting string.
        Recognized quotation marks are {}, "" and ''.
        '''

        start_quotes = ['{', '\'', '"']
        end_quotes = ['}', '\'', '"']

        val = value.strip()
        if len(val) < 2:
            return val

        chr = val[0]
        try: idx = start_quotes.index(chr)
        except: return val

        if val[-1] != end_quotes[idx]:
            return val

        unquoted_val = val[1:-1]
        return unquoted_val

    def scan_line(self, options: Options, line):
        '''Scan a file for test directives.

        Scans a file looking for lines starting and ending with string matching
        test-dir-start and test-dir-end containing directives.


        Returns a tuple (key, value) or None if no directive was found.

        '''

        result = self.directive_re.match(line)
        if result == None:
            return None

        key, value = result.group('key', 'value')

        if key == None or value == None:
            return None

        key = self.process_directive_key(options, key)
        value = self.process_directive_value(options, value)

        directive = (key, value)
        info("Test directive: " + str(directive) + "\n",
             options.verbosity() > 0)

        return directive


    def scan_file(self, options, filename):
        '''Scan a file for test directives.

        Scans a file looking for lines starting and ending with string matching
        test-dir-start and test-dir-end containing directives.

        Returns a directive-value map.

        '''
        # scan_file
        if self.directive_re == None:
            return []

        # Iterate over the file, recording the test directives found.
        info("Scanning " + filename + "\n", options.verbosity() > 0)

        try: tfile = open(filename, encoding = "utf-8")
        except:
            info("Can't open " + filename + " to scan for test directives\n",
                 options.verbosity() > 0)
            return {}

        test_dirs = []
        try:
            for line in tfile:
                directive = self.scan_line(options, line)
                if directive != None:
                    test_dirs.append(directive)
        except IOError as error:
            info("Can't scan " + filename + " for test directives\n",
                 options.verbosity() > 0)
        except UnicodeDecodeError:
            info("Can't scan " + filename + " for test directives\n",
                 options.verbosity() > 0)

        if test_dirs == []:
            info("No test directive found in " + filename + ".\n",
                 options.verbosity() > 1)

        tfile.close()

        return dict(test_dirs)


    def isignore(self, value: str) -> bool:
        '''Test whether a value indicates a file to ignore.'''

        if value != None and 'ignore' in value:
            return True
        return False

    def get_testers(self, testfile: TestFile):
        '''Get the testers for a test-file.'''

        testers = []
        test_type = testfile.test_type
        if test_type != None:
            # Get the tester specified in the test file or by file-suffix.
            testers = [ self.named_tester_table[t]
                        for t in test_type if t in self.named_tester_table ]

            if testers == []:
                warning("No tester found for " + testfile + "\n")

            return testers

        # No test type specified, get the testers for the test file based on the
        # file suffix.
        (_, testfile_full_suffix) = os.path.splitext(testfile.filename)
        testfile_suffix = testfile_full_suffix[1:]
        if testfile_suffix in self.tester_table:
            testers = self.tester_table[testfile_suffix]

        # Append the testers that have no suffix specified
        testers += self.other_testers

        return testers

    def cleanup(self, files):
        '''Delete built objects'''
        for f in files:
            if os.path.exists(f):
                os.remove(f)

    def run(self, options, files, wdir = None):

        '''Run testers on a set of files.

        Returns (num_tests, passes, fails, skipped).

        '''

        num_tests = 0
        passes = 0
        fails = 0
        skipped = 0

        for tfile in files:

            # Scan the test file for directives.
            directives = self.scan_file(options, tfile)

            # Setup a file record.
            tfile_record = TestFile(tfile, directives)

            if self.isignore(tfile_record.test_type):
                info("ignoring file " + tfile + "\n",
                     options.verbosity() > 1)
                continue

            testers = self.get_testers(tfile_record)
            if testers == []:
                info("No tester for " + tfile + "\n", options.verbosity() > 1)
                continue

            # Run each tester.
            for tester in testers:

                if tester.desc != None:
                    desc_str = 'Tester ' + tester.name
                    desc_str += ' - ' + tester.desc
                    info(desc_str + '\n', options.verbosity() > 0)

                    if tester.invalid:
                        warning('invalid tester, skipping.\n',
                                options.verbosity() > 0)
                        continue

                (status, srcfiles, srcs,
                 program, objs, extras) = tester.test(tfile_record)

                num_tests += 1
                if status == Tester.Status.PASS:
                    passes += 1
                elif status == Tester.Status.SKIP:
                    skipped += 1
                else:
                    fails += 1

                if (status != Tester.Status.FAIL
                    and not options.keep_temps()):
                    self.cleanup([program])
                    self.cleanup(objs)
                    self.cleanup(extras)

        return (num_tests, passes, fails, skipped)


    def run_on_dir(self, options, wdir):
        '''Run tester on the files in a directory.

        Returns (num_tests, passes, fails, skipped).

        '''

        info('directory: ' + wdir + '\n', options.verbosity() > 0)

        # Get the files in the directory.
        dir_files = os.listdir(path = wdir)
        dir_files.sort()

        # Run the tests on the files.
        return self.run(options, dir_files, wdir)

    def run_on_files(self, options, file_list, wdir = None):
        '''Run tester on a list of file names, possibly relative to a directory.

        The file list can include regexps recognized by glob.

        Returns (num_tests, passes, fails, skipped).

        '''

        num_tests = passes = fails = skipped = 0

        # Run the tests on the files.
        for tfile in file_list:
            if os.path.isdir(tfile):
                (ntests, ps, fs, sks) = self.run_on_dir(options, tfile)
            else:
                dir_files = glob.iglob(tfile)
                if wdir == None:
                    wdir = os.getcwd()
                (ntests, ps, fs, sks) = self.run(options, dir_files, wdir)

            num_tests += ntests
            passes += ps
            fails += fs
            skipped += sks

        return (num_tests, passes, fails, skipped)

###
# Testers

class CTester(Tester):
    '''Tester for C programs'''

    def default_config(self):
        return {
            'file-suffix': 'c',
            'files': '*.c',
            'object-suffix': '.o',
            'compiler': 'gcc -c -o {object} {flags} {source}',
            'linker': 'gcc -o {program} {flags} {objects} {libs}',
            'comment-start': '/*',
            'comment-end': '*/',
        }

    def __init__(self):
        super().__init__('c', desc = 'Test C programs')

class CXXTester(Tester):
    '''Tester for C++ programs'''

    def default_config(self):
        '''Initialize the C++ configuration.'''
        return {
            'inherit': 'c',
            'file-suffix': 'cpp,cxx',
            'files': '*.cpp',
            'object-suffix': '.o',
            'compiler': 'g++ -c -o {object} {flags} {source}',
            'linker': 'g++ -o {program} {flags} {objects} {libs}',
        }

    def __init__(self):
        super().__init__('c++', desc = 'Test C++ programs')

class OCamlTester(Tester):
    '''Tester for OCaml byte-code programs'''

    def default_config(self):
        '''Initialize the ocaml configuration.'''
        return {
            'file-suffix': 'ml',
            'files': '*.ml',
            'object-suffix': '.cmo',
            'compiler': 'ocamlc -c -o {object} {flags} {source}',
            'linker': 'ocamlc -o {program} {flags} {objects} {libs}',
        }

    def extra_object_files(self, srcfile: str, objfile: str):
        '''Auxiliary files built when compiling ocaml byte-code files.'''
        (name, ext) = os.path.splitext(srcfile)
        cmifile = name + '.cmi'
        return [cmifile]

    def make_object_name(self, filename: str):
        '''Make an OCaml byte-code object file name.'''

        (name, ext) = os.path.splitext(filename)
        if ext == '.mli':
            objext = '.cmi'
        else:
            objext = self.config.object_suffix

        object_name = self.make_unique_filename(name, objext)

        return object_name

    def compile_file(self, compiler, flags,
                     srcfile: str, wdir:str = None,
                     log = sys.stdout):
        '''Compile an ocaml file.

        Compiles file [srcfile] by calling [Tester.compile_file]. If [srcfile]
        is an .ml file, also checks for a matching .mli file and compile that if
        it exists.

        '''

        # Test for an .ml file.
        results = []
        (name, ext) = os.path.splitext(srcfile)
        if ext == '.ml':
            intfsrc = name + '.mli'
            if os.path.isfile(intfsrc):
                intf_results = Tester.compile_file(self, compiler, flags,
                                                   intfsrc, wdir, log)
                for (src, intf, _) in intf_results:
                    results.append((src, intf, True))

        results += Tester.compile_file(self, compiler, flags,
                                       srcfile, wdir, log)

        return results

    def __init__(self, name = 'ocaml', desc = 'Test OCaml programs',
                 config = None):
        super().__init__(name, desc, config)

class OCamlNativeTester(OCamlTester):
    '''Tester for OCaml native-code programs'''

    def default_config(self):
        '''Initialize the ocaml-native configuration.'''
        return {
            'file-suffix': 'ml',
            'files': '*.ml',
            'object-suffix': '.cmx',
            'compiler': 'ocamlopt -c -o {object} {flags} {source}',
            'linker': 'ocamlopt -o {program} {flags} {objects} {libs}',
        }

    def extra_object_files(self, srcfile: str, objfile: str):
        '''Auxiliary files built when compiling ocaml byte-code files.'''

        (name, ext) = os.path.splitext(srcfile)
        cmifile = name + '.' + 'cmi'
        ofile = name + '.' + 'o'
        extra = [cmifile, ofile]

        intfsrc = name + '.mli'
        if os.path.isfile(intfsrc):
            extra.append(name + '.o')

        return extra

    def __init__(self):
        super().__init__('ocaml-native', desc = 'Test OCaml-native programs')


# The list of tests.
test_list = [CTester(), CXXTester(), OCamlTester(), OCamlNativeTester()]

###
# Toplevel

## Set up arguments

def make_cmdline_parser():

    '''Construct a parser for command line options.

    Returns an instance of argparse.ArgumentParser.'''

    # Construct the parser.
    parser = argparse.ArgumentParser(description = 'Test driver.')

    parser.set_defaults(verbose = 0)
    parser.set_defaults(keep_temps = False)

    # Add options.
    parser.add_argument('--config', metavar = 'file',
                        action = 'store', type = str, dest = 'configfile',
                        help = 'Read configuration from <file>.')
    parser.add_argument('--print-config',
                        action = 'store_const', const = True,
                        help = 'Print the configuration being used.')
    parser.add_argument('--keep-temps',
                        action = 'store_const', const = True,
                        dest = 'keep_temps',
                        help = 'Keep all built programs and objects.')

    parser.add_argument('-v', '--verbose',
                        action = 'store_const', const = 1,
                        help = 'Verbose output.')
    parser.add_argument('--verbosity', metavar = 'n',
                        action = 'store', type = int, dest = 'verbose',
                        help =
                        'Verbose output. Larger values of <n>'
                        ' increase the verbosity of the output.')

    parser.add_argument('files', nargs = '*',
                        help = 'Files and/or directories to test.')

    # Done.
    return parser

def parse_cmdline(argv):
    '''Parse command line options.

    Parse command line arguments, returning a table with values for all options
    that are settable. Options that are not set on the command line are set to
    sensible values.
    '''
    parser = make_cmdline_parser()
    args = parser.parse_args(argv[1:])
    return args

def process_options(options: Options):
    '''Process the command line options, taking appropriate actions.

    Returns True iff nothing more needs to be done.'''

    if options.args.print_config == True:
        print_config(options.config)
        return True

    return False

def make_config(program = None, tester = None):
    '''Make a default configuration.

    If program is given, it is (name, config) and records the driver
    configuration to be stored under name.

    If tester is given, it is (name, config) and records the default tester
    configuration to be stored under name.

    '''

    config = configparser.ConfigParser()

    if program != None:
        (name, cfg) = program
        config[name] = cfg

    if tester != None:
        (name, cfg) = tester
        config[name] = cfg

    return config

def init_testers_config(testers, config = None):
    '''Initialize a test configuration for a list of testers.
    '''

    if config == None:
        config = configparser.ConfigParser()

    for tstr in testers:
        config[tstr.name] = tstr.default_config()

    return config

def print_config(config):

    ''' Print a configuration.'''
    config.write(sys.stdout)

def main(argv):

    '''Toplevel function.'''

    # Parse the command line.
    opts = parse_cmdline(argv)

    # Setup the configuration.
    config = make_config(program = (TestDriver.default_driver_name(),
                                    TestDriver.default_driver_config()),
                         tester = (Tester.default_tester_name(),
                                   Tester.default_tester_config()))

    config = init_testers_config(test_list, config)

    if opts.configfile != None:
        config.read([opts.configfile])

    # Make the options package.
    options = Options(program(), opts, config)

    # Process the arguments.
    if process_options(options):
        exit(0)

    # Create a driver.
    driver = TestDriver(options, test_list)

    # Run the tests in the current directory.
    if options.files() == []:
        files = ['.']
    else:
        files = options.files()

    (tests, passes, fails, skipped) = driver.run_on_files(options, files)

    report('tests: ' + str(tests) + ', '
           + 'passes: ' + str(passes) + ', '
           + 'failures: ' + str(fails) + ', '
           + 'skipped: ' + str(skipped) + '\n')

    retcode = 0
    if fails != 0:
        retcode = -1

    # Exit.
    exit(retcode)

if __name__ == "__main__":
    main(sys.argv)
