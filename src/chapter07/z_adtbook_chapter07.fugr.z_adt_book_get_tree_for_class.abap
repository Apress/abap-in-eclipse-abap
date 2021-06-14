FUNCTION Z_ADT_BOOK_GET_TREE_FOR_CLASS.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(CLASS_NAME) TYPE  SEOCPDKEY-CLSNAME
*"  EXPORTING
*"     VALUE(TREE) TYPE  SNODETAB
*"----------------------------------------------------------------------
  tree = NEW zcl_adt_book_tree_creator( )->create_tree( class_name ).

ENDFUNCTION.
