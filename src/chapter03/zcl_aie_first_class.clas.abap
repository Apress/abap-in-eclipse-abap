

"! <p class="shorttext synchronized" lang="en">Class for tests</p>
class zcl_aie_first_class definition
  public.


  public section.
    interfaces: if_oo_adt_classrun.

    methods constructor.
  protected section.
  private section.
    data: value_to_watch type sy-uzeit.
    "##TODO test

endclass.




class zcl_aie_first_class implementation.


  method constructor.
    value_to_watch = sy-uzeit.
  endmethod.


  method if_oo_adt_classrun~main.
    out->write( 'Run successfull' ).
  endmethod.
endclass.
