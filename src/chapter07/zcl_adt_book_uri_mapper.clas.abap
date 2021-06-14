class zcl_adt_book_uri_mapper definition
  public
  final
  create private .

  public section.
    class-methods: get_instance returning value(uri_mapper) type ref to zcl_adt_book_uri_mapper.
    methods: get_uri_for_tree_node importing value(node)       type snodetext
                                             value(class_name) type seocpdkey-clsname
                                   returning value(uri)        type string.
  protected section.
  private section.
    class-data: uri_mapper type ref to zcl_adt_book_uri_mapper.
    methods build_object_name
      importing
        node               type ref to snodetext
      returning
        value(object_name) type eu_lname.
    methods build_enclosed_object
      importing
        class_name             type seocpdkey-clsname
        node                   type ref to snodetext
      returning
        value(enclosed_object) type char70.
    methods update_original_class_name
      importing
        class_name type seocpdkey-clsname
      changing
        uri        type string.
    methods get_origin_class_name
      importing
                node                     type snodetext
                class_name               type seocpdkey-clsname
                objtype                  type seu_objtyp
      returning value(origin_class_name) type seocpdkey-clsname.
    methods build_node_type
      importing
        node             type snodetext
      returning
        value(node_type) type seu_type .

endclass.



class zcl_adt_book_uri_mapper implementation.
  method get_instance.
    if zcl_adt_book_uri_mapper=>uri_mapper is not bound.
      zcl_adt_book_uri_mapper=>uri_mapper = new #( ).
    endif.
    uri_mapper = zcl_adt_book_uri_mapper=>uri_mapper.
  endmethod.

  method build_node_type.
    node_type = node-type.
    if node-type+1(3) eq 'OT' and node-text8 is not initial.
      node_type+1(3) = 'IT'.
    endif.
    if node-type+1(3) eq 'OA' and node-text8 is not initial.
      node_type+1(3) = 'IA'.
    endif.
  endmethod.

  method get_uri_for_tree_node.
    check node-type(1) ne 'C'.
    node-type = build_node_type( node ).
    data(wb) = cl_wb_object=>create_from_toolaccess_key(
             p_object_type           = conv #( node-type+1(3) )
             p_object_name           = build_object_name( ref #( node ) )
             p_enclosing_object      = build_enclosed_object(
                                                    class_name = class_name
                                                    node       = ref #( node ) )
             ).
    if sy-subrc eq 0.
      wb->get_request_key(
        importing
          p_object_type     =   data(objtype)
          p_object_name     =   data(objkey)
        exceptions
          key_not_available = 1
          others            = 2
      ).
      if sy-subrc eq 0.

        cl_wb_request=>create_from_object_ref(
          exporting
            p_wb_object       = wb
          receiving
            p_wb_request      = data(wb_request)
          exceptions
            illegal_operation = 1
           cancelled         = 2
            others            = 3
        ).
        if sy-subrc eq 0.
          try.
              data(adt_reference) = cl_adt_tools_core_factory=>get_instance( )->get_uri_mapper( )->map_wb_request_to_objref( wb_request ).
              uri = adt_reference->ref_data-uri.

              class_name = get_origin_class_name( node       = node
                                                  class_name = class_name
                                                  objtype    = objtype ).
              update_original_class_name( exporting class_name = class_name
                                          changing uri = uri ).

            catch cx_adt_uri_mapping.
              "handle exception
          endtry.


        endif.
      endif.
    endif.
  endmethod.

  method get_origin_class_name.
    origin_class_name = class_name.
    case objtype.
      when swbm_c_type_cls_attribute or swbm_c_type_cls_mtd_def or swbm_c_type_cls_evt.
        data componentkey type seocmpkey.
        call function 'SEO_COMPONENT_BY_INHERITANCE'
          exporting
            cpdkey       = value seocpdkey( clsname = class_name cpdname = build_object_name( ref #( node ) ) )
          importing
            cmpkey       = componentkey
          exceptions
            not_existing = 1
            model_only   = 2
            others       = 3.
        if ( sy-subrc = 0 ).
          origin_class_name = componentkey-clsname.
        endif.
      when swbm_c_type_cls_mtd_impl.
        data methodkey type seocpdkey.
        methodkey = value #( clsname = class_name cpdname = build_object_name( ref #( node ) )  ).
        translate methodkey to upper case.

        call function 'SEO_METHOD_GET_YOUNGEST'
          exporting
            cpdkey       = methodkey
          importing
            youngest     = methodkey
          exceptions
            not_existing = 1
            others       = 2.
        if ( sy-subrc = 0 ).
          origin_class_name = methodkey-clsname.
        endif.
    endcase.

  endmethod.



  method update_original_class_name.

    replace first occurrence of regex '/sap/bc/adt/oo/classes/(.*)/source/'
                in uri with |/sap/bc/adt/oo/classes/{ to_lower( class_name ) }/source/|.

  endmethod.

  method build_object_name.
    case node->type+1(3).
      when swbm_c_type_cls_local_type or
           swbm_c_type_cls_local_intf or
           swbm_c_type_cls_lintf_meth or
           swbm_c_type_cls_lintf_attr or
           swbm_c_type_cls_lintf_event or
           swbm_c_type_cls_lintf_type or
           swbm_c_type_cls_lintf_intf .
        object_name = |{ node->text9+4(36) }                                   { node->text1 }|.
      when swbm_c_type_cls_mtd_impl or swbm_c_type_intf_type.
           object_name = node->text8.
      when swbm_c_type_intf_type or
           swbm_c_type_intf_attribute.
        split node->text8 at '~' into data(object) object_name.
      when others.
        object_name = node->text1 .
    endcase.

  endmethod.


  method build_enclosed_object.
    case node->type+1(3).
      when   swbm_c_type_cls_loc_meth_def or
             swbm_c_type_cls_loc_meth_impl  or
             swbm_c_type_cls_local_attr     or
             swbm_c_type_cls_local_event    or
             swbm_c_type_cls_local_type2    or
             swbm_c_type_cls_local_intf2 or
             swbm_c_type_cls_lintf_attr or
             swbm_c_type_cls_lintf_event or
             swbm_c_type_cls_lintf_type or
             swbm_c_type_cls_lintf_intf.

        enclosed_object = node->text8.
      when swbm_c_type_intf_type or
           swbm_c_type_intf_attribute.
        split node->text8 at '~' into enclosed_object data(object_name).
      when others.
        enclosed_object = class_name.
    endcase.
  endmethod.


endclass.

