class zcl_aie_second_class definition
  public
  inheriting from zcl_aie_first_class
  final
  create public .

  public section.
    methods: constructor.
  protected section.
  data: stop type true.

  private section.
  ENDCLASS.



CLASS ZCL_AIE_SECOND_CLASS IMPLEMENTATION.


  method constructor.
    super->constructor( test = abap_false  ).
    data(test) = conv string( text-001 ).
  endmethod.
ENDCLASS.
