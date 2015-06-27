#! /usr/bin/python
'''
:mod:`ocarina_common_tools` -- Tools used by Python binding
to the Ocarina AADL processor
==============================================================

.. moduleauthor:: Jerome Hugues, Arnaud Schach

This module provides tools to be used by the Python scripts
form the Python binding to the Ocarina AADL processor.

'''

################################################################################

try:
    import sys
    import StringIO
    from contextlib import contextmanager
    import ctypes
    import io
    import os
    import tempfile
    import platform

except ImportError:
    pass

################################################################################
def runOcarinaFunction (f, *parameters):
    info = io.BytesIO()
    error = io.BytesIO()
    raisedError = []
    res = ''
    with std_redirector(info,error):
        try:
            res = f (*parameters)
        except:
            raisedError.append(getErrorMessage())
    stderrMsg = sortStderrMessages(error.getvalue().decode('utf-8'))
    if stderrMsg[1]!=[]:
        raisedError.append(stderrMsg[1])
    return [ res , info.getvalue().decode('utf-8'), stderrMsg[0] ,
        raisedError ]

################################################################################

def getErrorMessage ():
    '''Get the error message from the raised error
    '''
    keep = False
    msg = ''
    for line in StringIO.StringIO(sys.exc_info()[1]):
        if line.lower().startswith('message:'):
            keep = True
        if line.lower().startswith('call stack traceback locations:'):
            break
        if keep:
            msg = msg + line[9:] + '\n'
    return msg

################################################################################

def sortStderrMessages (messages):
    '''Get the error and warning messages from the stderr

    :param messages: the messages written on stderr

    return a pair of the form [ warnings , errors ]

    '''

    msgType = 'error'
    warningMsg = ''
    errorMsg = ''
    warningMsgList = []
    errorMsgList = []
    for line in StringIO.StringIO(messages):
        if line.lower().startswith('error:'):
            if warningMsg.strip()!='':
                warningMsgList.append(warningMsg.strip())
                warningMsg = ''
            if errorMsg.strip()!='':
                errorMsgList.append(errorMsg.strip())
                errorMsg = ''
            msgType = 'error'
            errorMsg = warningMsg + line[7:] + '\n'
        elif line.lower().startswith('warning:'):
            if warningMsg.strip()!='':
                warningMsgList.append(warningMsg.strip())
                warningMsg = ''
            if errorMsg.strip()!='':
                errorMsgList.append(errorMsg.strip())
                errorMsg = ''
            msgType = 'warning'
            warningMsg = warningMsg + line[9:] + '\n'
        else:
            if msgType == 'warning':
                warningMsg = warningMsg + line + '\n'
            elif msgType == 'error':
                errorMsg = errorMsg + line + '\n'
    if warningMsg.strip()!='':
        warningMsgList.append(warningMsg.strip())
    if errorMsg.strip()!='':
        errorMsgList.append(errorMsg.strip())
    return [ warningMsgList , errorMsgList ]

################################################################################

@contextmanager
def std_redirector(stdoutStream, stderrStream):

    libc = ctypes.CDLL(None)

    # Note: Darwin (OS X) does ont export stdout/stderr as symbols,
    # but exports __stdoutp/__stderrp instead

    if platform.system () == "Darwin":
        c_stdout = ctypes.c_void_p.in_dll(libc, '__stdoutp')
        c_stderr = ctypes.c_void_p.in_dll(libc, '__stderrp')
    else:
        c_stdout = ctypes.c_void_p.in_dll(libc, 'stdout')
        c_stderr = ctypes.c_void_p.in_dll(libc, 'stderr')

    original_stdout_fd = sys.stdout.fileno()
    original_stderr_fd = sys.stderr.fileno()

    def _redirect_stdout(to_fd):
        libc.fflush(c_stdout)
        sys.stdout.close()
        os.dup2(to_fd, original_stdout_fd)
        sys.stdout = os.fdopen(original_stdout_fd, 'wb')

    def _redirect_stderr(to_fd):
        libc.fflush(c_stderr)
        sys.stderr.close()
        os.dup2(to_fd, original_stderr_fd)
        sys.stderr = os.fdopen(original_stderr_fd, 'wb')

    saved_stdout_fd = os.dup(original_stdout_fd)
    saved_stderr_fd = os.dup(original_stderr_fd)
    try:
        stdoutfile = tempfile.TemporaryFile(mode='w+b')
        _redirect_stdout(stdoutfile.fileno())
        stderrfile = tempfile.TemporaryFile(mode='w+b')
        _redirect_stderr(stderrfile.fileno())
        yield
        _redirect_stdout(saved_stdout_fd)
        _redirect_stderr(saved_stderr_fd)

        stdoutfile.flush()
        stdoutfile.seek(0, io.SEEK_SET)
        stdoutStream.write(stdoutfile.read())
        stderrfile.flush()
        stderrfile.seek(0, io.SEEK_SET)
        stderrStream.write(stderrfile.read())
    finally:
        stdoutfile.close()
        os.close(saved_stdout_fd)
        stderrfile.close()
        os.close(saved_stderr_fd)
