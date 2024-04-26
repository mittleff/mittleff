#include <Python.h>

/* void */
/* pyffi_Py_Initialize (void) */
/* { */
/*     PyStatus status; */

/*     PyConfig config; */
/*     PyConfig_InitPythonConfig(&config); */

/*     status = Py_InitializeFromConfig(&config); */
/*     if (PyStatus_Exception(status)) { */
/*         PyConfig_Clear(&config); */
/*         Py_ExitStatusException(status); */
/*     } */
/* } */

/* void */
/* Py_InitializeFromConfig (PyConfig config) */
/* { */
/*     PyConfig_Clear(&config); */
/* } */

PyObject*
pyffi_PyImport_ImportModuleEx (const char *name,
                               PyObject *globals,
                               PyObject *locals,
                               PyObject *fromlist)    
{
   return PyImport_ImportModuleEx(name, globals, locals, fromlist);
}

char*
pyffi_PyErr_Occurred_toCString (void)
{
  PyObject *exc, *str;

  exc = PyErr_Occurred();
  str = PyObject_Str(exc);

  Py_DecRef(exc);

  return PyUnicode_AsUTF8(str);
}
