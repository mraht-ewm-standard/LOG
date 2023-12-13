*"* use this source file for your ABAP unit test classes
CLASS ltc_log DEFINITION FOR TESTING RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    METHODS test FOR TESTING.

ENDCLASS.



CLASS ltc_log IMPLEMENTATION.

  METHOD test.

    zial_cl_log=>create( iv_object    = 'ZIAL_LOG'
                         iv_subobject = 'MFS'
                         iv_extnumber = |AbapUnitTest: LCL_LOG| ).

    zial_cl_log=>get( )->success( iv_msgtx = |A success message|
                                  it_msgde = VALUE #( ( fnam = 'HUIDENT' low = '123456' ) ) ).

    zial_cl_log=>save( ).

  ENDMETHOD.

ENDCLASS.
