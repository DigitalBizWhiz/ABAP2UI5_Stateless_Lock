CLASS z2ui5_cl_lock_request DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES tv_name           TYPE enqu_name.
    TYPES tv_table_name     TYPE tabname.
    TYPES tv_parameter_name TYPE fieldname.
    TYPES tv_mode           TYPE enqmode.
    TYPES tv_scope          TYPE ddenqscope.
    TYPES tv_wait           TYPE ddenqwait.
    TYPES tv_synchronous    TYPE ddenqsync.
    TYPES: BEGIN OF ts_table_mode,
             table_name TYPE tv_table_name,
             mode       TYPE tv_mode,
           END OF ts_table_mode.
    TYPES tt_table_mode TYPE HASHED TABLE OF ts_table_mode WITH UNIQUE KEY table_name.
    TYPES: BEGIN OF ts_parameter,
             name  TYPE tv_parameter_name,
             value TYPE REF TO data,
           END OF ts_parameter.
    TYPES tt_parameter TYPE HASHED TABLE OF ts_parameter WITH UNIQUE KEY name.
    TYPES: BEGIN OF ts_lock_parameter,
             name  TYPE fieldname,
             value TYPE fieldvalue,
           END OF ts_lock_parameter.
    TYPES: tt_lock_parameter TYPE STANDARD TABLE OF ts_lock_parameter WITH DEFAULT KEY.
    TYPES tt_z2ui5_t_lock_req TYPE STANDARD TABLE OF z2ui5_t_lock_req WITH DEFAULT KEY.
    TYPES: BEGIN OF ts_message,
             ef_message_class  TYPE msg_cls,
             ef_message_number TYPE msgnr,
             ef_message_text   TYPE natxt,
             ef_message_type   TYPE char1,
           END OF ts_message.
    CONSTANTS: BEGIN OF cs_mode,
                 write_lock                    TYPE tv_mode VALUE 'E',
                 shared_lock                   TYPE tv_mode VALUE 'S',
                 exclusive_lock                TYPE tv_mode VALUE 'X',
                 optimistic_lock               TYPE tv_mode VALUE 'O',
                 promote_optimistic_lock       TYPE tv_mode VALUE 'R',
                 check_write_lock              TYPE tv_mode VALUE 'V',
                 check_shared_lock             TYPE tv_mode VALUE 'W',
                 check_exclusive_lock          TYPE tv_mode VALUE 'U',
                 check_promote_optimistic_lock TYPE tv_mode VALUE 'C',
               END OF cs_mode.
    CONSTANTS: BEGIN OF cs_scope,
                 no_update_program              TYPE tv_scope VALUE '1',
                 update_program                 TYPE tv_scope VALUE '2',
                 interactive_and_update_program TYPE tv_scope VALUE '3',
               END OF cs_scope.
    CONSTANTS: BEGIN OF cs_synchronous,
                 yes TYPE tv_synchronous VALUE 'X',
                 no  TYPE tv_synchronous VALUE ' ',
               END OF cs_synchronous.
    CONSTANTS: BEGIN OF cs_wait,
                 no  TYPE tv_wait VALUE ' ',
                 yes TYPE tv_wait VALUE 'X',
               END OF cs_wait.

    DATA: gf_lock_object    TYPE enqu_name,
          gt_lock_parameter TYPE tt_lock_parameter,
          gt_table_mode     TYPE tt_table_mode,
          gt_parameter      TYPE tt_parameter.

    METHODS:
      constructor
        IMPORTING
          it_table_mode     TYPE tt_table_mode
          if_lock_object    TYPE enqu_name
          it_lock_parameter TYPE tt_lock_parameter,
      enqueue
        IMPORTING
          if_scope        TYPE ddenqscope DEFAULT cs_scope-update_program
          if_wait         TYPE ddenqwait DEFAULT cs_wait-no
          if_username_job TYPE uname
        EXCEPTIONS
          cx_abap_foreign_lock
          cx_ditat_lock_system_failure
          cx_pfm_pap_create_job,
      dequeue
        IMPORTING
          if_scope       TYPE ddenqwait
          if_synchronous TYPE ddenqsync
        EXCEPTIONS
          cx_abap_foreign_lock
          cx_ditat_lock_system_failure,
      get_lock
        RETURNING  VALUE(rv_is_locked) TYPE abap_bool
        EXCEPTIONS
                   cx_abap_foreign_lock
                   cx_ditat_lock_system_failure
                   cx_nwecm_lock_not_found.

  PRIVATE SECTION.

ENDCLASS.



CLASS Z2UI5_CL_LOCK_REQUEST IMPLEMENTATION.


  METHOD constructor.
    gt_table_mode = it_table_mode.
    LOOP AT gt_table_mode ASSIGNING FIELD-SYMBOL(<entry>).
      <entry>-mode = 'O'.
    ENDLOOP.
    gf_lock_object = if_lock_object.
    gt_lock_parameter = it_lock_parameter.
    LOOP AT gt_lock_parameter ASSIGNING FIELD-SYMBOL(<ls_lock_parameter>).
      IF sy-tabix = 1 AND <ls_lock_parameter>-name <> 'MANDT'.
        INSERT VALUE ts_parameter( name  = 'MANDT'
                                   value = REF #( sy-mandt ) ) INTO TABLE gt_parameter.
      ENDIF.
      INSERT VALUE ts_parameter( name  = <ls_lock_parameter>-name
                                 value = REF #( <ls_lock_parameter>-value ) ) INTO TABLE gt_parameter.
    ENDLOOP.
  ENDMETHOD.


  METHOD enqueue.
    DATA lf_jobname TYPE btcjob.
    DATA lf_jobcount           TYPE btcjobcnt.
    DATA lf_startdate          TYPE btcsdate.
    DATA lf_starttime          TYPE btcstime.
    DATA ls_locked_entry       TYPE z2ui5_t_lock_req.
    DATA: lf_enqkey TYPE vim_enqkey.
    LOOP AT gt_lock_parameter INTO DATA(ls_lock_parameter).
      IF sy-tabix = 1 AND ls_lock_parameter-name <> 'MANDT'.
        CONCATENATE lf_enqkey sy-mandt INTO lf_enqkey.
      ENDIF.
      CONCATENATE lf_enqkey ls_lock_parameter-value INTO lf_enqkey.
    ENDLOOP.
    CALL FUNCTION 'ENQUEUE_E_TABLE'
      EXPORTING
        tabname        = 'Z2UI5_T_LOCK_REQ'
        varkey         = lf_enqkey
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN '1'.
          RAISE cx_abap_foreign_lock.
        WHEN '2'.
          RAISE cx_ditat_lock_system_failure.
      ENDCASE.
    ENDIF.
    TRY.
        DATA(lo_lock) = cl_abap_lock_object_factory=>get_instance( gf_lock_object ).
        lo_lock->enqueue( it_table_mode = gt_table_mode
                          it_parameter  = gt_parameter
                          _scope = cs_scope-no_update_program
                          _wait = cs_wait-no ).
        DATA: ls_z2ui5_t_lock_req TYPE z2ui5_t_lock_req.
        ls_z2ui5_t_lock_req-mandt = sy-mandt.
        ls_z2ui5_t_lock_req-requester = sy-uname.
        GET TIME STAMP FIELD ls_z2ui5_t_lock_req-timestamp_request.
        ls_z2ui5_t_lock_req-name_lock_object = gf_lock_object.
        ls_z2ui5_t_lock_req-key_lock_object = lf_enqkey.
        ls_z2ui5_t_lock_req-signal_shared_lock_request = abap_true.
        MODIFY z2ui5_t_lock_req FROM ls_z2ui5_t_lock_req.
        COMMIT WORK AND WAIT.
        ls_locked_entry = ls_z2ui5_t_lock_req.
      CATCH cx_abap_foreign_lock.
        RAISE cx_abap_foreign_lock.
      CATCH cx_abap_lock_failure.
        RAISE cx_ditat_lock_system_failure.
    ENDTRY.
    CONCATENATE 'Z2UI5_LOCK_MANAGER_' if_username_job INTO lf_jobname.
    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = lf_jobname
      IMPORTING
        jobcount         = lf_jobcount
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      RAISE cx_pfm_pap_create_job.
    ENDIF.
    DATA lt_sel_par TYPE STANDARD TABLE OF rsparams WITH DEFAULT KEY.
    APPEND VALUE #( selname = 'P_WAIT'
                    kind    = 'P'
                    sign    = 'I'
                    option  = 'EQ'
                    low     = '2' ) TO lt_sel_par.
    APPEND VALUE #( selname = 'P_MAX_T'
                    kind    = 'P'
                    sign    = 'I'
                    option  = 'EQ'
                    low     = '305.0' ) TO lt_sel_par.
    SUBMIT z2ui5_lock_manager USER if_username_job
           WITH SELECTION-TABLE lt_sel_par
           VIA JOB lf_jobname NUMBER lf_jobcount
           AND RETURN.
    IF sy-subrc = 0.
      lf_startdate = sy-datum.
      lf_starttime = sy-uzeit.
      DATA: ls_jobhead TYPE tbtcjob.
      DATA: lt_steplist TYPE STANDARD TABLE OF tbtcstep WITH DEFAULT KEY.
      CALL FUNCTION 'BP_JOB_READ'
        EXPORTING
          job_read_jobcount     = lf_jobcount
          job_read_jobname      = lf_jobname
          job_read_opcode       = '20'
        IMPORTING
          job_read_jobhead      = ls_jobhead
        TABLES
          job_read_steplist     = lt_steplist
        EXCEPTIONS
          invalid_opcode        = 1
          job_doesnt_exist      = 2
          job_doesnt_have_steps = 3
          OTHERS                = 4.
      IF sy-subrc <> 0.
        RAISE cx_pfm_pap_create_job.
      ENDIF.
      ls_jobhead-sdluname = if_username_job.
      CALL FUNCTION 'BP_JOB_MODIFY'
        EXPORTING
          dialog           = 'N'
          jobcount         = ls_jobhead-jobcount
          jobname          = ls_jobhead-jobname
          new_jobhead      = ls_jobhead
          opcode           = '16'
        IMPORTING
          modified_jobhead = ls_jobhead
        TABLES
          new_steplist     = lt_steplist.
      IF sy-subrc <> 0.
        RAISE cx_pfm_pap_create_job.
      ENDIF.
      CALL FUNCTION 'JOB_CLOSE'
        EXPORTING
          jobcount             = lf_jobcount
          jobname              = lf_jobname
          sdlstrtdt            = lf_startdate
          sdlstrttm            = lf_starttime
          strtimmed            = 'X'
        EXCEPTIONS
          cant_start_immediate = 1
          invalid_startdate    = 2
          jobname_missing      = 3
          job_close_failed     = 4
          job_nosteps          = 5
          job_notex            = 6
          lock_failed          = 7
          invalid_target       = 8
          invalid_time_zone    = 9
          OTHERS               = 10.
      IF sy-subrc <> 0.
        RAISE cx_pfm_pap_create_job.
      ENDIF.
    ELSE.
      RAISE cx_pfm_pap_create_job.
    ENDIF.
    IF NOT ls_locked_entry IS INITIAL.
      DATA(ls_info) = lcl_wait=>wait( if_time = '10.0'
                                  if_user = sy-uname
                                  if_wait = '1'
                                  if_field_to_check = 'SHARED_LOCK_DONE'
                                  if_field_set = abap_true
                                  is_z2ui5_t_lock_req = ls_locked_entry ).
    ENDIF.
    TRY.
        lo_lock->dequeue( it_table_mode = gt_table_mode
                          it_parameter  = gt_parameter
                          _scope = cs_scope-no_update_program
                          _synchronous = cs_synchronous-yes ).
      CATCH cx_abap_foreign_lock.
        RAISE cx_abap_foreign_lock.
      CATCH cx_abap_lock_failure.
        RAISE cx_ditat_lock_system_failure.
    ENDTRY.
    IF ls_info-condition = abap_true.
      ls_locked_entry-signal_exclusive_lock_request = 'X'.
      UPDATE z2ui5_t_lock_req FROM ls_locked_entry.
      COMMIT WORK.
    ELSE.
      RAISE cx_ditat_lock_system_failure.
    ENDIF.
  ENDMETHOD.


  METHOD dequeue.
    DATA: lf_enqkey TYPE vim_enqkey.
    LOOP AT gt_lock_parameter INTO DATA(ls_lock_parameter).
      IF sy-tabix = 1 AND ls_lock_parameter-name <> 'MANDT'.
        CONCATENATE lf_enqkey sy-mandt INTO lf_enqkey.
      ENDIF.
      CONCATENATE lf_enqkey ls_lock_parameter-value INTO lf_enqkey.
    ENDLOOP.
    CALL FUNCTION 'ENQUEUE_E_TABLE'
      EXPORTING
        tabname        = 'Z2UI5_T_LOCK_REQ'
        varkey         = lf_enqkey
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN '1'.
          RAISE cx_abap_foreign_lock.
        WHEN '2'.
          RAISE cx_ditat_lock_system_failure.
      ENDCASE.
    ENDIF.
    TRY.
        DATA(lo_lock) = cl_abap_lock_object_factory=>get_instance( gf_lock_object ).
        lo_lock->dequeue( it_table_mode = gt_table_mode
                          it_parameter  = gt_parameter
                          _scope        = cs_scope-interactive_and_update_program
                          _synchronous  = cs_synchronous-no ).
        DELETE FROM z2ui5_t_lock_req WHERE requester = sy-uname AND
                                           name_lock_object = gf_lock_object AND
                                           key_lock_object = lf_enqkey.
        COMMIT WORK AND WAIT.
      CATCH cx_abap_foreign_lock.
        RAISE cx_abap_foreign_lock.
      CATCH cx_abap_lock_failure.
        RAISE cx_ditat_lock_system_failure.
    ENDTRY.
  ENDMETHOD.


  METHOD get_lock.
    DATA lf_enqkey TYPE vim_enqkey.
    LOOP AT gt_lock_parameter INTO DATA(ls_lock_parameter).
      IF sy-tabix = 1 AND ls_lock_parameter-name <> 'MANDT'.
        CONCATENATE lf_enqkey sy-mandt INTO lf_enqkey.
      ENDIF.
      CONCATENATE lf_enqkey ls_lock_parameter-value INTO lf_enqkey.
    ENDLOOP.
    CALL FUNCTION 'ENQUEUE_E_TABLE'
      EXPORTING
        tabname        = 'Z2UI5_T_LOCK_REQ'
        varkey         = lf_enqkey
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN '1'.
          RAISE cx_abap_foreign_lock.
        WHEN '2'.
          RAISE cx_ditat_lock_system_failure.
      ENDCASE.
    ENDIF.
    SELECT *
    FROM z2ui5_t_lock_req
    INTO TABLE @DATA(lt_z2ui5_t_lock_req)
    WHERE requester = @sy-uname AND
          key_lock_object = @lf_enqkey AND
          name_lock_object = @gf_lock_object AND
          shared_lock_done = @abap_true AND
          exclusive_lock_done = @abap_true.
    IF lines( lt_z2ui5_t_lock_req ) = 0.
      RAISE cx_nwecm_lock_not_found.
    ENDIF.
    DATA(ls_z2ui5_t_lock_req) = lt_z2ui5_t_lock_req[ 1 ].
    ls_z2ui5_t_lock_req-signal_exclusive_lock_release = abap_true.
    UPDATE z2ui5_t_lock_req FROM ls_z2ui5_t_lock_req.
    COMMIT WORK AND WAIT.
    DATA(ls_info) = lcl_wait=>wait( if_time = '10.0'
                            if_user = sy-uname
                            if_wait = '1'
                            if_field_to_check = 'EXCLUSIVE_LOCK_DONE'
                            if_field_set = abap_false
                            is_z2ui5_t_lock_req = ls_z2ui5_t_lock_req ).
    IF ls_info-condition = abap_false.
      RAISE cx_ditat_lock_system_failure.
    ENDIF.
    TRY.
        DATA(lo_lock) = cl_abap_lock_object_factory=>get_instance( gf_lock_object ).
        lo_lock->enqueue( it_table_mode = gt_table_mode
                          it_parameter  = gt_parameter
                          _scope = cs_scope-update_program
                          _wait = cs_wait-no ).
        ls_z2ui5_t_lock_req-exclusive_lock_done = abap_false.
        ls_z2ui5_t_lock_req-signal_shared_lock_release = abap_true.
        UPDATE z2ui5_t_lock_req FROM ls_z2ui5_t_lock_req.
        COMMIT WORK AND WAIT.
        ls_info = lcl_wait=>wait( if_time = '10.0'
                        if_user = sy-uname
                        if_wait = '1'
                        if_field_to_check = 'SHARED_LOCK_DONE'
                        if_field_set = abap_false
                        is_z2ui5_t_lock_req = ls_z2ui5_t_lock_req ).
        IF ls_info-condition = abap_false.
          RAISE cx_ditat_lock_system_failure.
        ENDIF.
        DATA(lt_table_mode) = gt_table_mode[].
        LOOP AT lt_table_mode ASSIGNING FIELD-SYMBOL(<entry>).
          <entry>-mode = 'R'.
        ENDLOOP.
        lo_lock->enqueue( it_table_mode = lt_table_mode
                          it_parameter  = gt_parameter
                          _scope = cs_scope-update_program
                          _wait = cs_wait-yes ).
      CATCH cx_abap_foreign_lock.
        RAISE cx_abap_foreign_lock.
      CATCH cx_abap_lock_failure.
        RAISE cx_ditat_lock_system_failure.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
