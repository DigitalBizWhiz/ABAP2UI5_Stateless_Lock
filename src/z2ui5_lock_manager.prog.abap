*&---------------------------------------------------------------------*
*& Report Z2UI5_LOCK_MANAGER
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

REPORT z2ui5_lock_manager.

INCLUDE z2ui5_lock_manager_top                  .
INCLUDE z2ui5_lock_manager_p01                  .

START-OF-SELECTION.
  PARAMETERS: p_wait  TYPE int4 DEFAULT 2,
              p_max_t TYPE tzntstmpl DEFAULT '305.0'.
  DATA(lv_ts) = VALUE timestampl( ).
  DATA: lf_job_required TYPE abap_bool VALUE abap_false.
  CALL FUNCTION 'ENQUEUE_E_TABLE'
    EXPORTING
      tabname        = 'Z2UI5_T_LOCK_REQ'
      varkey         = 'JOB'
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc = 0.
    lf_job_required = abap_true.
  ENDIF.
  IF lf_job_required = abap_true.
    DATA(lv_info) = lcl_wait=>wait( iv_max_time = p_max_t
                                    iv_wait_time = p_wait ).
  ENDIF.
