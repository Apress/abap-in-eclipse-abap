CLASS zcl_exceptions_catch DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS: constructor RAISING cx_demo_constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS intialize RAISING cx_sy_itab_line_not_found.
    METHODS test RAISING cx_sadl_entity_not_found.
ENDCLASS.



CLASS zcl_exceptions_catch IMPLEMENTATION.
  METHOD constructor.
    TRY.
        intialize( ).
      CATCH cx_root INTO DATA(e).
        RAISE EXCEPTION TYPE cx_demo_constructor.
    ENDTRY.
  ENDMETHOD.


  METHOD intialize.
    TRY.
        test( ).
      CATCH cx_root INTO DATA(e).
        RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDTRY.
  ENDMETHOD.

  METHOD test.
    IF 1 <> 2.
      RAISE EXCEPTION TYPE cx_sadl_entity_not_found.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
