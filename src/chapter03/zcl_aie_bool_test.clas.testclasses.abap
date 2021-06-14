*"* use this source file for your ABAP unit test classes
class ltcl_tetes definition final for testing
  duration short
  risk level harmless.

  private section.
    methods:
      first_test for testing raising cx_static_check.
endclass.


class ltcl_tetes implementation.

  method first_test.
    cl_abap_unit_assert=>assert_equals(
      exporting
        act                  = 1
        exp                  = 1
    ).
  endmethod.

endclass.
