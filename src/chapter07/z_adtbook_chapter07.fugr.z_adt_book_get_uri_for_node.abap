FUNCTION Z_ADT_BOOK_GET_URI_FOR_NODE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(NODE) TYPE  SNODETEXT
*"     VALUE(CLASS_NAME) TYPE  SEOCPDKEY-CLSNAME
*"  EXPORTING
*"     VALUE(URI) TYPE  STRING
*"----------------------------------------------------------------------
  uri =  zcl_adt_book_uri_mapper=>get_instance( )->get_uri_for_tree_node( EXPORTING node  = node
                                                                               class_name = class_name
                                                                    ).

ENDFUNCTION.
