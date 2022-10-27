*"* use this source file for your ABAP unit test classes
CLASS lcl_log DEFINITION FOR TESTING RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    METHODS test FOR TESTING.

ENDCLASS.



CLASS lcl_log IMPLEMENTATION.

  METHOD test.

    zial_cl_log=>init( iv_object    = 'ZIAL_LOG'
                       iv_subobject = 'MFS'
                       iv_extnumber = |AbapUnitTest: LCL_LOG| ).

    zial_cl_log=>get( )->success( msgtx = |A success message| ).

    zial_cl_log=>save( ).

  ENDMETHOD.

ENDCLASS.
