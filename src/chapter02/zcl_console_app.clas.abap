class zcl_console_app definition
  public
  final
  create public .

  public section.

    interfaces if_oo_adt_classrun .
  protected section.
  private section.
ENDCLASS.



CLASS ZCL_CONSOLE_APP IMPLEMENTATION.


  method if_oo_adt_classrun~main.
    out->write_text( 'This is the first line'  ).
    out->write_text( 'This is the second line'  ).
    out->write_text( 'This is the third line'  ).
  endmethod.
ENDCLASS.
