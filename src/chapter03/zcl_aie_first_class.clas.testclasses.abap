*"* use this source file for your ABAP unit test classes
class ltcl_my_test_class definition final for testing
  duration short
  risk level harmless.

  private section.
    methods:
      first_test for testing raising cx_static_check.
endclass.


class ltcl_my_test_class implementation.

  method first_test.
    cl_abap_unit_assert=>assert_true(
      exporting
        act              = abap_true
    ).
  endmethod.

endclass.
