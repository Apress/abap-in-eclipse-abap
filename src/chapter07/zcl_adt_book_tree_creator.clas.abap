class zcl_adt_book_tree_creator definition
  public
  final
  create public .

  public section.
    methods: create_tree importing class_name  type seocpdkey-clsname
                         returning value(tree) type snodetab.
  protected section.
  private section.
endclass.



class zcl_adt_book_tree_creator implementation.

  method create_tree.
    call function 'WB_ANYTYPE_RETURN_OBJECT_LIST'
      exporting
        p_object_type        = 'CLAS'
        p_object_name        = class_name
      tables
        nodetab              = tree
      exceptions
        objectlist_not_found = 1
        others               = 2.
    if sy-subrc <> 0.
*  message id sy-msgid type sy-msgty number sy-msgno
*    with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.
  endmethod.
endclass.
