CLASS zcl_adt_book_tree_uri_res DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS post REDEFINITION.

    CONSTANTS: class_name    TYPE seoclname VALUE 'ZCL_ADT_BOOK_TREE_URI_RES',
               resource_type TYPE string VALUE 'SNODETEXT'.

  PRIVATE SECTION.
    DATA: factory TYPE REF TO cl_adt_rest_cnt_hdl_factory.
    CONSTANTS class_tree_node_transformation TYPE string VALUE 'Z_ADTBOOK_CLASS_TREE_NODE' ##NO_TEXT.
    METHODS get_class_name_from_uri
      IMPORTING request           TYPE REF TO  if_adt_rest_request
      RETURNING VALUE(class_name) TYPE seoclname.
    METHODS get_content_handler
      RETURNING VALUE(result) TYPE REF TO if_adt_rest_content_handler .
    METHODS get_uri_for_tree_node
      IMPORTING
        VALUE(class_name) TYPE seoclname
        VALUE(node)       TYPE snodetext
      RETURNING
        VALUE(uri)        TYPE string.
ENDCLASS.



CLASS zcl_adt_book_tree_uri_res IMPLEMENTATION.

  METHOD post.

    factory = cl_adt_rest_cnt_hdl_factory=>get_instance( ).

    DATA: node TYPE snodetext.
    request->get_body_data(
      EXPORTING
        content_handler = get_content_handler( )
      IMPORTING
        data            = node  ).

    response->set_body_data(
      EXPORTING
        content_handler = factory->get_handler_for_plain_text( )
        data = get_uri_for_tree_node(
                            node = node
                            class_name = get_class_name_from_uri( request ) )
    ).

  ENDMETHOD.

  METHOD get_uri_for_tree_node.

    CALL FUNCTION 'Z_ADT_BOOK_GET_URI_FOR_NODE'
      EXPORTING
        node       = node
        class_name = class_name
      IMPORTING
        uri        = uri.

  ENDMETHOD.


  METHOD get_content_handler.
    result = factory->get_handler_for_xml_using_st(
                EXPORTING
                        st_name      = class_tree_node_transformation
                        root_name    = resource_type ).
  ENDMETHOD.

  METHOD get_class_name_from_uri.
    request->get_uri_attribute( EXPORTING name = 'class_name'
                                         mandatory = abap_true
                               IMPORTING value = class_name ).

  ENDMETHOD.

ENDCLASS.

