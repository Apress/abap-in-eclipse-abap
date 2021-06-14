CLASS zcl_adt_book_class_tree_res DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS get REDEFINITION.

    CONSTANTS: class_name    TYPE seoclname VALUE 'ZCL_ADT_BOOK_CLASS_TREE_RES',
               resource_type TYPE string VALUE 'SNODETAB'.


  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS class_tree_transformation TYPE string VALUE 'Z_ADTBOOK_CLASS_TREE' ##NO_TEXT.
    METHODS get_class_name_from_uri
      IMPORTING request           TYPE REF TO  if_adt_rest_request
      RETURNING VALUE(class_name) TYPE seoclname.
    METHODS get_class_tree
      IMPORTING
                class_name        TYPE seoclname
      RETURNING VALUE(class_tree) TYPE snodetab.
    METHODS get_content_handler
      RETURNING VALUE(result) TYPE REF TO if_adt_rest_content_handler .
ENDCLASS.



CLASS zcl_adt_book_class_tree_res IMPLEMENTATION.

  METHOD get.
    DATA(class_tree) = get_class_tree( get_class_name_from_uri( request ) ).
    response->set_body_data(
      EXPORTING
        content_handler =  get_content_handler( )
        data            =  class_tree
    ).
  ENDMETHOD.

  METHOD get_class_tree.
    CALL FUNCTION 'Z_ADT_BOOK_GET_TREE_FOR_CLASS'
      EXPORTING
        class_name = class_name
      IMPORTING
        tree       = class_tree.

  ENDMETHOD.


  METHOD get_content_handler.
    DATA(factory) = cl_adt_rest_cnt_hdl_factory=>get_instance( ).
     result = factory->get_handler_for_xml_using_st(
                EXPORTING
                        st_name      = class_tree_transformation
                        root_name    = resource_type ).
  ENDMETHOD.

  METHOD get_class_name_from_uri.
    request->get_uri_attribute( EXPORTING name = 'class_name'
                                         mandatory = abap_true
                               IMPORTING value = class_name ).

  ENDMETHOD.

ENDCLASS.

