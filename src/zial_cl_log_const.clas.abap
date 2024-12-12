CLASS zial_cl_log_const DEFINITION
  PUBLIC ABSTRACT FINAL.

  PUBLIC SECTION.
    CONSTANTS: BEGIN OF mc_default,
                 class_name TYPE seoclsname VALUE zial_cl_log_sap=>mc_default_sap-class_name,
               END OF mc_default.

ENDCLASS.


CLASS zial_cl_log_const IMPLEMENTATION.
ENDCLASS.
