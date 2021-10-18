FUNCTION z_adtco_get_uri_for_tree_node.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(NODE) TYPE  SNODETEXT
*"     VALUE(OBJECT_NAME) TYPE  EU_LNAME
*"     VALUE(OBJECT_TYPE) TYPE  SEU_OBJ
*"  EXPORTING
*"     VALUE(URI) TYPE  STRING
*"----------------------------------------------------------------------
  uri =  zcl_adtco_uri_mapper=>get_instance( )->get_uri_for_tree_node( EXPORTING node  = node
                                                                                 object_name = object_name
                                                                                 object_type = object_type
                                                                    ).




ENDFUNCTION.
