*&---------------------------------------------------------------------*
*& Include          Z2UI5_LOCK_MANAGER_P01
*&---------------------------------------------------------------------*
CLASS lcl_wait DEFINITION.
  PUBLIC SECTION.
    TYPES : BEGIN OF ty_s_tmstmp,
              v_process_start TYPE timestampl,
              v_start         TYPE timestampl,
              v_now           TYPE timestampl,
              v_elapsed       TYPE tzntstmpl,
              v_limit         TYPE tzntstmpl,
              v_condition     TYPE abap_bool,
            END OF ty_s_tmstmp.
    CLASS-METHODS: wait
      IMPORTING
                iv_max_time    TYPE tzntstmpl
                iv_wait_time   TYPE int4
      RETURNING VALUE(rs_info) TYPE ty_s_tmstmp.
  PRIVATE SECTION.
    CLASS-METHODS: condition_code
      IMPORTING iv_start     TYPE timestampl
                iv_now       TYPE timestampl
                iv_limit     TYPE tzntstmpl
      RETURNING VALUE(rv_ok) TYPE abap_bool.

    CLASS-METHODS: enqueue_lock
      IMPORTING it_table_mode  TYPE tt_table_mode
                it_parameter   TYPE tt_parameter
                iv_lock_object TYPE enqu_name
                iv_scope       TYPE ddenqscope
                iv_wait        TYPE ddenqwait.
    CLASS-METHODS: dequeue_lock
      IMPORTING it_table_mode  TYPE tt_table_mode
                it_parameter   TYPE tt_parameter
                iv_lock_object TYPE enqu_name
                iv_scope       TYPE ddenqscope
                iv_synchronous TYPE ddenqsync.
    CLASS-METHODS: get_lock_data
      IMPORTING iv_mode             TYPE enqmode
                is_z2ui5_t_lock_req TYPE z2ui5_t_lock_req
      RETURNING VALUE(rs_lock_data) TYPE ts_lock_data.
ENDCLASS.

CLASS lcl_wait IMPLEMENTATION.
  METHOD wait.
    rs_info = VALUE ty_s_tmstmp( v_elapsed = 0 v_limit = iv_max_time v_condition = abap_false ).
    GET TIME STAMP FIELD rs_info-v_process_start.
    rs_info-v_start = rs_info-v_process_start.
    GET TIME STAMP FIELD rs_info-v_now.
    WHILE rs_info-v_elapsed < rs_info-v_limit.
      IF abap_true = condition_code( iv_start = rs_info-v_start
                                     iv_now = rs_info-v_now
                                     iv_limit = rs_info-v_limit ).
        rs_info-v_condition = abap_true.
        EXIT.
      ENDIF.
      CALL FUNCTION 'ENQUE_SLEEP'
        EXPORTING
          seconds        = iv_wait_time
        EXCEPTIONS
          system_failure = 1
          OTHERS         = 2.
      SELECT MAX( timestamp_request )
      FROM z2ui5_t_lock_req
      INTO @DATA(lv_new_start).
      IF sy-subrc = 0 AND lv_new_start > rs_info-v_start.
        rs_info-v_start = lv_new_start.
      ENDIF.
      GET TIME STAMP FIELD rs_info-v_now.
      rs_info-v_elapsed = cl_abap_tstmp=>subtract( tstmp1 = rs_info-v_now tstmp2 = rs_info-v_start ).
    ENDWHILE.
    CALL FUNCTION 'DEQUEUE_E_TABLE'
      EXPORTING
        tabname   = 'Z2UI5_T_LOCK_REQ'
        varkey    = 'JOB'
        _synchron = 'X'
      EXCEPTIONS
        OTHERS    = 01.
    DATA: lt_parameter       TYPE tt_parameter.
    DATA: ls_z2ui5_t_lock_req TYPE z2ui5_t_lock_req.
    DATA: lt_z2ui5_t_lock_req TYPE tt_z2ui5_t_lock_req.
    SELECT *
    FROM z2ui5_t_lock_req
    INTO TABLE @lt_z2ui5_t_lock_req
    WHERE timestamp_request <= @rs_info-v_start.
    LOOP AT lt_z2ui5_t_lock_req INTO ls_z2ui5_t_lock_req.
      DATA(ls_lock_data) =  get_lock_data( iv_mode = cs_mode-write_lock
                                           is_z2ui5_t_lock_req = ls_z2ui5_t_lock_req ).
      CLEAR lt_parameter.
      LOOP AT ls_lock_data-t_parameter ASSIGNING FIELD-SYMBOL(<ls_parameter>) WHERE name <> 'MANDT'.
        INSERT VALUE ts_parameter( name = <ls_parameter>-name value = REF #( <ls_parameter>-value ) ) INTO TABLE lt_parameter.
      ENDLOOP.
      dequeue_lock( it_table_mode = ls_lock_data-t_table_mode
                    it_parameter = lt_parameter
                    iv_lock_object = ls_lock_data-v_lock_object_name
                    iv_scope = cs_scope-interactive_and_update_program
                    iv_synchronous = cs_synchronous-yes ).
      ls_lock_data =  get_lock_data( iv_mode = cs_mode-optimistic_lock
                                     is_z2ui5_t_lock_req = ls_z2ui5_t_lock_req ).
      CLEAR lt_parameter.
      LOOP AT ls_lock_data-t_parameter ASSIGNING <ls_parameter> WHERE name <> 'MANDT'.
        INSERT VALUE ts_parameter( name = <ls_parameter>-name value = REF #( <ls_parameter>-value ) ) INTO TABLE lt_parameter.
      ENDLOOP.
      dequeue_lock( it_table_mode = ls_lock_data-t_table_mode
                    it_parameter = lt_parameter
                    iv_lock_object = ls_lock_data-v_lock_object_name
                    iv_scope = cs_scope-interactive_and_update_program
                    iv_synchronous = cs_synchronous-yes ).
      ls_z2ui5_t_lock_req-exclusive_lock_done = ' '.
      ls_z2ui5_t_lock_req-signal_exclusive_lock_request = ' '.
      ls_z2ui5_t_lock_req-signal_exclusive_lock_release = ' '.
      ls_z2ui5_t_lock_req-shared_lock_done = ' '.
      ls_z2ui5_t_lock_req-signal_shared_lock_request = ' '.
      ls_z2ui5_t_lock_req-signal_shared_lock_release = ' '.
      CALL FUNCTION 'Z2UI5_UPDATE_Z2UI5_T_LOCK_REQ' DESTINATION 'NONE'
        EXPORTING
          is_z2ui5_t_lock_req = ls_z2ui5_t_lock_req.
    ENDLOOP.
  ENDMETHOD.
*
  METHOD condition_code.
    DATA: lt_parameter       TYPE tt_parameter.
    DATA: lt_z2ui5_t_lock_req TYPE tt_z2ui5_t_lock_req.
    SELECT *
    FROM z2ui5_t_lock_req
    INTO TABLE @lt_z2ui5_t_lock_req.
    DATA: lf_lock_object_name TYPE enqu_name.
    DATA: lt_table_mode       TYPE tt_table_mode.
    DATA: lt_parameters       TYPE tt_parameter.
    DATA: ls_z2ui5_t_lock_req       TYPE z2ui5_t_lock_req.
    DATA(lf_application_active) = abap_false.
    LOOP AT lt_z2ui5_t_lock_req INTO DATA(ls_lock_entry).
      CLEAR lt_parameter.
      MOVE-CORRESPONDING ls_lock_entry TO ls_z2ui5_t_lock_req.
      DATA(lf_elapsed_position) = cl_abap_tstmp=>subtract( tstmp1 = iv_now tstmp2 = ls_z2ui5_t_lock_req-timestamp_request ).
      IF lf_elapsed_position < iv_limit.
        lf_application_active = abap_true.
      ENDIF.
      IF ( ls_lock_entry-signal_exclusive_lock_release = 'X' AND
        ls_lock_entry-signal_shared_lock_release = 'X' ) OR lf_elapsed_position >= iv_limit.
        lf_application_active = abap_true.

        DATA(ls_lock_data) =  get_lock_data( iv_mode = cs_mode-write_lock
                                             is_z2ui5_t_lock_req = ls_z2ui5_t_lock_req ).
        CLEAR lt_parameter.
        LOOP AT ls_lock_data-t_parameter ASSIGNING FIELD-SYMBOL(<ls_parameter>) WHERE name <> 'MANDT'.
          INSERT VALUE ts_parameter( name = <ls_parameter>-name value = REF #( <ls_parameter>-value ) ) INTO TABLE lt_parameter.
        ENDLOOP.
        dequeue_lock( it_table_mode = ls_lock_data-t_table_mode
                      it_parameter = lt_parameter
                      iv_lock_object = ls_lock_data-v_lock_object_name
                      iv_scope = cs_scope-interactive_and_update_program
                      iv_synchronous = cs_synchronous-yes ).

        ls_lock_data =  get_lock_data( iv_mode = cs_mode-optimistic_lock
                                       is_z2ui5_t_lock_req = ls_z2ui5_t_lock_req ).
        CLEAR lt_parameter.
        LOOP AT ls_lock_data-t_parameter ASSIGNING <ls_parameter> WHERE name <> 'MANDT'.
          INSERT VALUE ts_parameter( name = <ls_parameter>-name value = REF #( <ls_parameter>-value ) ) INTO TABLE lt_parameter.
        ENDLOOP.
        dequeue_lock( it_table_mode = ls_lock_data-t_table_mode
                      it_parameter = lt_parameter
                      iv_lock_object = ls_lock_data-v_lock_object_name
                      iv_scope = cs_scope-interactive_and_update_program
                      iv_synchronous = cs_synchronous-no ).
        ls_lock_entry-exclusive_lock_done = ' '.
        ls_lock_entry-signal_exclusive_lock_request = ' '.
        ls_lock_entry-signal_exclusive_lock_release = ' '.
        ls_lock_entry-shared_lock_done = ' '.
        ls_lock_entry-signal_shared_lock_request = ' '.
        ls_lock_entry-signal_shared_lock_release = ' '.
        CALL FUNCTION 'Z2UI5_UPDATE_Z2UI5_T_LOCK_REQ' DESTINATION 'NONE'
          EXPORTING
            is_z2ui5_t_lock_req = ls_lock_entry.
      ELSEIF ls_lock_entry-exclusive_lock_done = 'X' AND
             ls_lock_entry-shared_lock_done = 'X' AND
             ls_lock_entry-signal_exclusive_lock_release = 'X'.
        lf_application_active = abap_true.

        ls_lock_data =  get_lock_data( iv_mode = cs_mode-write_lock
                               is_z2ui5_t_lock_req = ls_z2ui5_t_lock_req ).
        CLEAR lt_parameter.
        LOOP AT ls_lock_data-t_parameter ASSIGNING <ls_parameter> WHERE name <> 'MANDT'.
          INSERT VALUE ts_parameter( name = <ls_parameter>-name value = REF #( <ls_parameter>-value ) ) INTO TABLE lt_parameter.
        ENDLOOP.
        dequeue_lock( it_table_mode = ls_lock_data-t_table_mode
                      it_parameter = lt_parameter
                      iv_lock_object = ls_lock_data-v_lock_object_name
                      iv_scope = cs_scope-interactive_and_update_program
                      iv_synchronous = cs_synchronous-no ).
        ls_lock_entry-exclusive_lock_done = ' '.
        ls_lock_entry-signal_exclusive_lock_release = ' '.
        CALL FUNCTION 'Z2UI5_UPDATE_Z2UI5_T_LOCK_REQ' DESTINATION 'NONE'
          EXPORTING
            is_z2ui5_t_lock_req = ls_lock_entry.
      ELSEIF ls_lock_entry-shared_lock_done = 'X' AND
             ls_lock_entry-signal_shared_lock_release = 'X'.
        lf_application_active = abap_true.

        ls_lock_data =  get_lock_data( iv_mode = cs_mode-optimistic_lock
                       is_z2ui5_t_lock_req = ls_z2ui5_t_lock_req ).
        CLEAR lt_parameter.
        LOOP AT ls_lock_data-t_parameter ASSIGNING <ls_parameter> WHERE name <> 'MANDT'.
          INSERT VALUE ts_parameter( name = <ls_parameter>-name value = REF #( <ls_parameter>-value ) ) INTO TABLE lt_parameter.
        ENDLOOP.
        dequeue_lock( it_table_mode = ls_lock_data-t_table_mode
                      it_parameter = lt_parameter
                      iv_lock_object = ls_lock_data-v_lock_object_name
                      iv_scope = cs_scope-interactive_and_update_program
                      iv_synchronous = cs_synchronous-no ).

        ls_lock_entry-shared_lock_done = ' '.
        ls_lock_entry-signal_shared_lock_release = ' '.
        CALL FUNCTION 'Z2UI5_UPDATE_Z2UI5_T_LOCK_REQ' DESTINATION 'NONE'
          EXPORTING
            is_z2ui5_t_lock_req = ls_lock_entry.
      ELSEIF ls_lock_entry-shared_lock_done = 'X' AND
             ls_lock_entry-signal_exclusive_lock_request = 'X'.
        lf_application_active = abap_true.

        ls_lock_data =  get_lock_data( iv_mode = cs_mode-promote_optimistic_lock
               is_z2ui5_t_lock_req = ls_z2ui5_t_lock_req ).
        CLEAR lt_parameter.
       LOOP AT ls_lock_data-t_parameter ASSIGNING <ls_parameter> WHERE name <> 'MANDT'.
          INSERT VALUE ts_parameter( name = <ls_parameter>-name value = REF #( <ls_parameter>-value ) ) INTO TABLE lt_parameter.
        ENDLOOP.
        enqueue_lock( it_table_mode = ls_lock_data-t_table_mode
                      it_parameter = lt_parameter
                      iv_lock_object = ls_lock_data-v_lock_object_name
                      iv_scope = cs_scope-update_program
                      iv_wait = cs_wait-no ).
        ls_lock_entry-exclusive_lock_done = 'X'.
        ls_lock_entry-signal_exclusive_lock_request = ' '.
        CALL FUNCTION 'Z2UI5_UPDATE_Z2UI5_T_LOCK_REQ' DESTINATION 'NONE'
          EXPORTING
            is_z2ui5_t_lock_req = ls_lock_entry.
      ELSEIF ls_lock_entry-signal_shared_lock_request = 'X'.
        lf_application_active = abap_true.
        ls_lock_data =  get_lock_data( iv_mode = cs_mode-optimistic_lock
                                       is_z2ui5_t_lock_req = ls_z2ui5_t_lock_req ).
        CLEAR lt_parameter.
        LOOP AT ls_lock_data-t_parameter ASSIGNING <ls_parameter> WHERE name <> 'MANDT'.
          INSERT VALUE ts_parameter( name = <ls_parameter>-name value = REF #( <ls_parameter>-value ) ) INTO TABLE lt_parameter.
        ENDLOOP.
*        enqueue_lock( it_table_mode = ls_lock_data-t_table_mode
*                      it_parameter = lt_parameter
*                      iv_lock_object = ls_lock_data-v_lock_object_name
*                      iv_scope = cs_scope-no_update_program
*                      iv_wait = cs_wait-no ).
        enqueue_lock( it_table_mode = ls_lock_data-t_table_mode
                      it_parameter = lt_parameter
                      iv_lock_object = ls_lock_data-v_lock_object_name
                      iv_scope = cs_scope-update_program
                      iv_wait = cs_wait-no ).
        ls_lock_entry-shared_lock_done = 'X'.
        ls_lock_entry-signal_shared_lock_request = ' '.

        CALL FUNCTION 'Z2UI5_UPDATE_Z2UI5_T_LOCK_REQ' DESTINATION 'NONE'
          EXPORTING
            is_z2ui5_t_lock_req = ls_lock_entry.
      ENDIF.
    ENDLOOP.
    DATA(lt_no_requests_positions) = VALUE tt_z2ui5_t_lock_req( FOR <fs> IN lt_z2ui5_t_lock_req WHERE ( exclusive_lock_done = abap_false AND
                                                                         signal_exclusive_lock_release = abap_false AND
                                                                         signal_exclusive_lock_request = abap_false AND
                                                                         shared_lock_done = abap_false AND
                                                                         signal_shared_lock_release = abap_false AND
                                                                         signal_shared_lock_request = abap_false )
                          ( <fs> ) ).
    IF lines( lt_no_requests_positions ) = lines( lt_z2ui5_t_lock_req ) AND lf_application_active = abap_false.
      CALL FUNCTION 'DEQUEUE_E_TABLE'
        EXPORTING
          tabname   = 'Z2UI5_T_LOCK_REQ'
          varkey    = 'JOB'
          _synchron = 'X'
        EXCEPTIONS
          OTHERS    = 01.
      rv_ok = abap_true.
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD enqueue_lock.
    TRY.
        DATA(lo_lock) = cl_abap_lock_object_factory=>get_instance( iv_lock_object ).
        lo_lock->enqueue( it_table_mode = it_table_mode
                          it_parameter  = it_parameter
                          _scope        = iv_scope
                          _wait         = iv_wait ).
      CATCH cx_abap_foreign_lock.
      CATCH cx_abap_lock_failure.
    ENDTRY.
  ENDMETHOD.

  METHOD dequeue_lock.
    TRY.
        DATA(lo_lock) = cl_abap_lock_object_factory=>get_instance( iv_lock_object ).
        lo_lock->dequeue( it_table_mode = it_table_mode
                          it_parameter  = it_parameter
                          _scope        = iv_scope
                          _synchronous  = iv_synchronous ).
      CATCH cx_abap_foreign_lock.
      CATCH cx_abap_lock_failure.
    ENDTRY.
  ENDMETHOD.

  METHOD get_lock_data.
    SELECT roottab
    FROM dd25l
    INTO @DATA(lv_roottab)
    UP TO 1 ROWS
    WHERE viewname = @is_z2ui5_t_lock_req-name_lock_object AND
          as4local = 'A'.
    ENDSELECT.
    SELECT fieldname, rollname, objpos
    FROM dd27s
    INTO TABLE @DATA(lt_fieldname_position)
    WHERE viewname = @is_z2ui5_t_lock_req-name_lock_object AND
          as4local = 'A' AND
          keyflag = 'X'.
    SORT lt_fieldname_position BY objpos ASCENDING.
    IF lines( lt_fieldname_position ) > 0.
      SELECT rollname, outputlen
      FROM dd04l
      INTO TABLE @DATA(lt_rollname_outputlen)
      FOR ALL ENTRIES IN @lt_fieldname_position
      WHERE rollname  = @lt_fieldname_position-rollname.
    ENDIF.
    DATA(lf_start_index) = 0.
    DATA(lf_sub_length) = 0.
    DATA: lf_sub_string TYPE fieldvalue.
    DATA: lt_parameter_helper TYPE tt_parameter_helper.
    LOOP AT lt_fieldname_position INTO DATA(ls_fieldname_position).
      READ TABLE lt_rollname_outputlen WITH KEY rollname = ls_fieldname_position-rollname INTO DATA(ls_rollname_outputlen).
      IF sy-subrc = 0.
        lf_sub_length = CONV i( ls_rollname_outputlen-outputlen ).
        IF strlen( is_z2ui5_t_lock_req-key_lock_object ) < lf_sub_length.
          lf_sub_length = strlen( is_z2ui5_t_lock_req-key_lock_object ).
        ENDIF.
        lf_sub_string = is_z2ui5_t_lock_req-key_lock_object+lf_start_index(lf_sub_length).
*        INSERT VALUE tt_parameter( name  = ls_fieldname_position-fieldname
*                                   value = lf_sub_string ) INTO TABLE rs_lock_data-t_parameter.
        lf_start_index = lf_start_index + lf_sub_length.
      ENDIF.
    ENDLOOP.
    rs_lock_data-v_lock_object_name = is_z2ui5_t_lock_req-name_lock_object.
    INSERT VALUE ts_table_mode( mode       = iv_mode
                                table_name = lv_roottab ) INTO TABLE rs_lock_data-t_table_mode.
  ENDMETHOD.
ENDCLASS.
