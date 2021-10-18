CLASS zcl_adtco_uri_mapper DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    CLASS-METHODS: get_instance RETURNING VALUE(uri_mapper) TYPE REF TO zcl_adtco_uri_mapper.
    METHODS: get_uri_for_tree_node IMPORTING VALUE(node)        TYPE snodetext
                                             VALUE(object_name) TYPE eu_lname
                                             VALUE(object_type) TYPE seu_obj
                                   RETURNING VALUE(uri)         TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA: uri_mapper TYPE REF TO zcl_adtco_uri_mapper.
    METHODS build_object_name
      IMPORTING
        node                 TYPE REF TO snodetext
      RETURNING
        VALUE(object_name)   TYPE eu_lname.
    METHODS build_enclosed_object
      IMPORTING
        object_name            TYPE eu_lname
        object_type            TYPE seu_obj
        node                   TYPE REF TO snodetext
      RETURNING
        VALUE(enclosed_object) TYPE char70.
    METHODS update_original_class_name
      IMPORTING
        class_name TYPE eu_lname
      CHANGING
        uri        TYPE string.
    METHODS get_origin_class_name
      IMPORTING
                node                     TYPE snodetext
                class_name               TYPE eu_lname
                objtype                  TYPE seu_objtyp
      RETURNING VALUE(origin_class_name) TYPE eu_lname.
    METHODS build_node_type
      IMPORTING
        node             TYPE snodetext
      RETURNING
        VALUE(node_type) TYPE seu_type .
    METHODS get_object_name
      IMPORTING
        original_object_name TYPE eu_lname
        original_object_type TYPE seu_obj
      RETURNING
        VALUE(object_name)   TYPE eu_lname.
    METHODS get_uri_directly
      IMPORTING
        node                 TYPE snodetext
        original_object_name TYPE eu_lname
        original_object_type TYPE seu_obj
      RETURNING
        VALUE(uri)           TYPE string.
    METHODS build_internal_name
      IMPORTING
        local_container_name TYPE seu_text
        local_object_name    TYPE seu_text
      RETURNING
        VALUE(internal_name) TYPE seu_text .
ENDCLASS.



CLASS zcl_adtco_uri_mapper IMPLEMENTATION.
  METHOD get_instance.
    IF zcl_adtco_uri_mapper=>uri_mapper IS NOT BOUND.
      zcl_adtco_uri_mapper=>uri_mapper = NEW #( ).
    ENDIF.
    uri_mapper = zcl_adtco_uri_mapper=>uri_mapper.
  ENDMETHOD.

  METHOD build_node_type.
    node_type = node-type.
    IF node-type+1(3) EQ 'OT' AND node-text8 IS NOT INITIAL.
      node_type+1(3) = 'IT'.
    ENDIF.
    IF node-type+1(3) EQ 'OA' AND node-text8 IS NOT INITIAL.
      node_type+1(3) = 'IA'.
    ENDIF.
        IF node-type EQ 'OONN' AND node-text8 IS NOT INITIAL.
      node_type = 'OPN'.
    ENDIF.
  ENDMETHOD.

  METHOD get_uri_for_tree_node.
    CHECK node-type(1) NE 'C'.

    node-type = build_node_type( node ).
    DATA(wb) = cl_wb_object=>create_from_toolaccess_key(
             p_object_type           = CONV #( node-type+1(3) )
             p_object_name           = build_object_name( REF #( node ) )
             p_enclosing_object      = build_enclosed_object(
                                                    object_name = object_name
                                                    object_type = object_type
                                                    node       = REF #( node ) )
             ).
    IF sy-subrc EQ 0.
      wb->get_request_key(
        IMPORTING
          p_object_type     = DATA(objtype)
          p_object_name     = DATA(objkey)
        EXCEPTIONS
          key_not_available = 1
          OTHERS            = 2
      ).
      IF sy-subrc EQ 0.

        cl_wb_request=>create_from_object_ref(
          EXPORTING
            p_wb_object       = wb
          RECEIVING
            p_wb_request      = DATA(wb_request)
          EXCEPTIONS
            illegal_operation = 1
            cancelled         = 2
            OTHERS            = 3
        ).
        IF sy-subrc EQ 0.
          TRY.
              DATA(adt_reference) = cl_adt_tools_core_factory=>get_instance( )->get_uri_mapper( )->map_wb_request_to_objref( wb_request ).
              uri = adt_reference->ref_data-uri.

              object_name = get_origin_class_name( node       = node
                                                   class_name = object_name
                                                   objtype    = objtype ).
              update_original_class_name( EXPORTING class_name = object_name
                                          CHANGING  uri        = uri ).

            CATCH cx_adt_uri_mapping.
              uri = get_uri_directly( node                 = node
                                      original_object_name = object_name
                                      original_object_TYPE = object_type ).
              IF uri IS NOT INITIAL.
                RETURN.
              ENDIF.
          ENDTRY.


        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_origin_class_name.
    origin_class_name = class_name.
    CASE objtype.
      WHEN swbm_c_type_cls_attribute OR swbm_c_type_cls_mtd_def OR swbm_c_type_cls_evt.
        DATA componentkey TYPE seocmpkey.
        CALL FUNCTION 'SEO_COMPONENT_BY_INHERITANCE'
          EXPORTING
            cpdkey       = VALUE seocpdkey( clsname = class_name cpdname = build_object_name( REF #( node ) ) )
          IMPORTING
            cmpkey       = componentkey
          EXCEPTIONS
            not_existing = 1
            model_only   = 2
            OTHERS       = 3.
        IF ( sy-subrc = 0 ).
          origin_class_name = componentkey-clsname.
        ENDIF.
      WHEN swbm_c_type_cls_mtd_impl.
        DATA methodkey TYPE seocpdkey.
        methodkey = VALUE #( clsname = class_name cpdname = build_object_name( REF #( node ) )  ).
        TRANSLATE methodkey TO UPPER CASE.

        CALL FUNCTION 'SEO_METHOD_GET_YOUNGEST'
          EXPORTING
            cpdkey       = methodkey
          IMPORTING
            youngest     = methodkey
          EXCEPTIONS
            not_existing = 1
            OTHERS       = 2.
        IF ( sy-subrc = 0 ).
          origin_class_name = methodkey-clsname.
        ENDIF.
    ENDCASE.

  ENDMETHOD.



  METHOD update_original_class_name.

    REPLACE FIRST OCCURRENCE OF REGEX '/sap/bc/adt/oo/classes/(.*)/source/'
                IN uri WITH |/sap/bc/adt/oo/classes/{ to_lower( class_name ) }/source/|.

  ENDMETHOD.

  METHOD build_object_name.
    CASE node->type+1(3).
      WHEN swbm_c_type_cls_local_type OR
           swbm_c_type_cls_local_intf OR
           swbm_c_type_cls_lintf_meth OR
           swbm_c_type_cls_lintf_attr OR
           swbm_c_type_cls_lintf_event OR
           swbm_c_type_cls_lintf_type OR
           swbm_c_type_cls_lintf_intf .
        object_name = |{ node->text9+4(36) }                                   { node->text1 }|.
      WHEN swbm_c_type_cls_mtd_impl OR swbm_c_type_intf_type.
        object_name = node->text8.
      WHEN swbm_c_type_intf_type OR
           swbm_c_type_intf_attribute.
        SPLIT node->text8 AT '~' INTO DATA(object) object_name.
        IF object_name IS INITIAL.
          IF strlen( node->text9 ) GE 4 AND node->text9(4) EQ 'INTF'.
            object_name = node->text9.
            SHIFT object_name BY 4 PLACES LEFT.
          ENDIF.
        ENDIF.
      WHEN OTHERS.
        object_name = node->text1 .
    ENDCASE.

  ENDMETHOD.


  METHOD build_enclosed_object.

    CASE node->type+1(3).
      WHEN   swbm_c_type_cls_loc_meth_def OR
             swbm_c_type_cls_loc_meth_impl  OR
             swbm_c_type_cls_local_attr     OR
             swbm_c_type_cls_local_event    OR
             swbm_c_type_cls_local_type2    OR
             swbm_c_type_cls_local_intf2 OR
             swbm_c_type_cls_lintf_attr OR
             swbm_c_type_cls_lintf_event OR
             swbm_c_type_cls_lintf_type OR
             swbm_c_type_cls_lintf_intf.
        enclosed_object = node->text8.
        IF object_type EQ 'FUGR/F'.
          enclosed_object(40) = object_name.
        ENDIF.
      WHEN swbm_c_type_intf_type OR
           swbm_c_type_intf_attribute.
        SPLIT node->text8 AT '~' INTO enclosed_object DATA(tmp_object_name).
      WHEN OTHERS.
        enclosed_object = get_object_name(
          original_object_name = object_name
          original_object_type = object_type
        ).

    ENDCASE.

  ENDMETHOD.

  METHOD get_object_name.
    CASE original_object_type.
      WHEN 'FUGR/FF'.
        SELECT SINGLE pname FROM tfdir
        INTO object_name
        WHERE funcname = original_object_name.
      WHEN 'FUGR/F'.
        object_name = |SAPL{  original_object_name }|.
      WHEN OTHERS.
        object_name = original_object_name.
    ENDCASE.
  ENDMETHOD.

  METHOD get_uri_directly.
    IF node-type EQ 'OONT' AND node-text9(4) EQ 'REPS'.
      uri = |/sap/bc/adt/programs/programs/{ node-text8(40) }/source/main#type=PROG%2FPNY;name={ build_internal_name(
                                                                                                   local_container_name = CONV #( node-text8+40(30) )
                                                                                                   local_object_name    = node-text1 )  }|.
    ELSEIF node-type EQ 'OOND' AND node-text9(4) EQ 'REPS'.
      uri = |/sap/bc/adt/programs/programs/{ node-text8(40) }/source/main#type=PROG%2FPNM;name={ build_internal_name(
                                                                                                   local_container_name = CONV #( node-text8+40(30) )
                                                                                                   local_object_name    = node-text1 )  }|.
    ELSEIF node-type EQ 'OONA' AND node-text9(4) EQ 'REPS'.
      uri = |/sap/bc/adt/programs/programs/{ node-text8(40) }/source/main#type=PROG%2FPNA;name={ build_internal_name(
                                                                                                   local_container_name = CONV #( node-text8+40(30) )
                                                                                                   local_object_name    = node-text1 )  }|.
    ELSEIF node-type EQ 'OONN' AND node-text9(4) EQ 'REPS'.
      uri = |/sap/bc/adt/programs/programs/{ node-text8(40) }/source/main#type=PROG%2FPNN;name={ build_internal_name(
                                                                                                   local_container_name = CONV #( node-text8+40(30) )
                                                                                                   local_object_name    = node-text1 )  }|.
    ELSEIF node-type EQ 'OOLD' AND node-text9(4) EQ 'REPS'.

      IF original_object_type CP 'FUGR/*'.
        uri = |/sap/bc/adt/programs/includes/{ node-text8(40) }/source/main?context=%2fsap%2fbc%2fadt%2ffunctions%2fgroups%2f{ original_object_name }#| &&
        |type=PROG%2FPLM;name={ escape( val = build_internal_name(
                                                                                                    local_container_name = CONV #( node-text8+40(30) )
                                                                                                    local_object_name    = node-text1 ) format = cl_abap_format=>e_uri )  }|.
      ELSE.
        uri = |/sap/bc/adt/programs/programs/{ node-text8(40) }/source/main#type=PROG%2FPLM;name={ escape( val = build_internal_name(
                                                                                                     local_container_name = CONV #( node-text8+40(30) )
                                                                                                     local_object_name    = node-text1 ) format = cl_abap_format=>e_uri )  }|.
      ENDIF.
    ELSEIF node-type EQ 'OOLI' AND node-text9(4) EQ 'REPS'.
      IF original_object_type  CP 'FUGR/*'.
        uri = |/sap/bc/adt/programs/includes/{ node-text8(40) }/source/main?context=%2fsap%2fbc%2fadt%2ffunctions%2fgroups%2f{ original_object_name }#| &&
        |type=PROG%2FPLM;name={ escape( val = build_internal_name(
                                                                                                    local_container_name = CONV #( node-text8+40(30) )
                                                                                                    local_object_name    = node-text1 ) format = cl_abap_format=>e_uri )  }|.
      ELSE.
        uri = |/sap/bc/adt/programs/programs/{ node-text8(40) }/source/main#type=PROG%2FPLM;name={ build_internal_name(
                                                                                                     local_container_name = CONV #( node-text8+40(30) )
                                                                                                     local_object_name    = node-text1 )  }|.
      ENDIF.
    ELSEIF node-type EQ 'OOLA' AND node-text9(4) EQ 'REPS'.
      IF original_object_type CP 'FUGR/*'.
        uri = |/sap/bc/adt/programs/includes/{ node-text8(40) }/source/main?context=%2fsap%2fbc%2fadt%2ffunctions%2fgroups%2f{ original_object_name }#| &&
   |type=PROG%2FPLA;name={ escape( val = build_internal_name(
                                                                                               local_container_name = CONV #( node-text8+40(30) )
                                                                                               local_object_name    = node-text1 ) format = cl_abap_format=>e_uri )  }|.
      ELSE.
        uri = |/sap/bc/adt/programs/programs/{ node-text8(40) }/source/main#type=PROG%2FPLA;name={ build_internal_name(
                                                                                                     local_container_name = CONV #( node-text8+40(30) )
                                                                                                     local_object_name    = node-text1 )  }|.
      ENDIF.
    ELSEIF node-type EQ 'OOLT' AND node-text9(4) EQ 'REPS'.
      uri = |/sap/bc/adt/programs/programs/{ node-text8(40) }/source/main#type=PROG%2FPLY;name={ build_internal_name(
                                                                                                   local_container_name = CONV #( node-text8+40(30) )
                                                                                                   local_object_name    = node-text1 )  }|.
    ELSEIF node-type EQ 'OOLN' AND node-text9(4) EQ 'REPS'.
      uri = |/sap/bc/adt/programs/programs/{ node-text8(40) }/source/main#type=PROG%2FPN;name={  node-text1  }|.
    ENDIF.



  ENDMETHOD.

  METHOD build_internal_name.
    internal_name(30) = local_container_name.
    internal_name+30(32) = local_object_name.
  ENDMETHOD.

ENDCLASS.

