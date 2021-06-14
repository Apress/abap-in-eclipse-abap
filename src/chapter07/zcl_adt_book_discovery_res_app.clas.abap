CLASS zcl_adt_book_discovery_res_app DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_disc_res_app_base
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS root_scheme TYPE string VALUE 'http://abapblog.com/adt' ##NO_TEXT.
    CONSTANTS class_tree_uri TYPE string VALUE '/classtree/{class_name}' ##NO_TEXT.
    CONSTANTS tree_node_uri TYPE string VALUE '/treeuri/{class_name}' ##NO_TEXT.
    CONSTANTS root_rel_scheme TYPE string VALUE 'http://abapblog.com/adt/relations' ##NO_TEXT.
    CONSTANTS xml_application TYPE string VALUE 'application/xml' ##NO_TEXT.
    CONSTANTS application_title TYPE string VALUE 'ADT Discovery for Class Outline' ##NO_TEXT.

    CONSTANTS: BEGIN OF category,
                 classtree TYPE string VALUE 'classtree' ##NO_TEXT,
                 treeuri   TYPE string VALUE 'treeuri' ##NO_TEXT,
               END OF category,
               BEGIN OF description,
                 classtree TYPE string VALUE 'Class Tree' ##NO_TEXT,
                 treeuri   TYPE string VALUE 'Tree URI' ##NO_TEXT,
               END OF description,
               static_uri     TYPE string VALUE '/adtbook' ##NO_TEXT,
               discovery_path TYPE string VALUE '/discovery' ##NO_TEXT.

    METHODS if_adt_rest_rfc_application~get_static_uri_path REDEFINITION.

  PROTECTED SECTION.
    METHODS: get_application_title REDEFINITION,
      register_resources REDEFINITION.
  PRIVATE SECTION.
    METHODS register_tree_uri_resource
      IMPORTING
        registry TYPE REF TO if_adt_disc_rest_rc_registry.
    METHODS register_class_tree_resource
      IMPORTING
        registry TYPE REF TO if_adt_disc_rest_rc_registry.
ENDCLASS.



CLASS zcl_adt_book_discovery_res_app IMPLEMENTATION.

  METHOD if_adt_rest_rfc_application~get_static_uri_path.

    result = static_uri.

  ENDMETHOD.

  METHOD get_application_title.
    result = application_title.
  ENDMETHOD.

  METHOD register_resources.

    register_class_tree_resource( registry ).
    register_tree_uri_resource( registry ).

  ENDMETHOD.

  METHOD register_class_tree_resource.

    DATA(collection) = registry->register_discoverable_resource(
      url             = ''
      handler_class   = ''
      description     = description-classtree
      category_scheme = root_scheme
      category_term   = category-classtree
    ).

    collection->register_disc_res_w_template(
                   relation = root_rel_scheme
                   template = class_tree_uri
                   description = description-classtree
                   type = xml_application
                   handler_class = zcl_adt_book_class_tree_res=>class_name
                ).

  ENDMETHOD.

  METHOD register_tree_uri_resource.

    DATA(collection) = registry->register_discoverable_resource(
      url             = ''
      handler_class   = ''
      description     = description-treeuri
      category_scheme = root_scheme
      category_term   = category-treeuri
    ).

    collection->register_disc_res_w_template(
                   relation = root_rel_scheme
                   template = tree_node_uri
                   description = description-treeuri
                   type = xml_application
                   handler_class = zcl_adt_book_tree_uri_res=>class_name
                ).

  ENDMETHOD.

ENDCLASS.

