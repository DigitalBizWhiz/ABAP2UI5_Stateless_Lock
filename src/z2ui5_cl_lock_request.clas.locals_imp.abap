*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_wait DEFINITION DEFERRED.

CLASS lcl_wait DEFINITION.
  PUBLIC SECTION.
    TYPES : tt_z2ui5_t_lock_req TYPE STANDARD TABLE OF z2ui5_t_lock_req WITH DEFAULT KEY.
    TYPES : BEGIN OF ty_s_tmstmp,
              start     TYPE timestampl,
              now       TYPE timestampl,
              elapsed   TYPE tzntstmpl,
              limit     TYPE tzntstmpl,
              condition TYPE abap_bool,
            END OF ty_s_tmstmp.
    CLASS-METHODS wait
      IMPORTING if_time             TYPE tzntstmpl
                if_user             TYPE uname
                if_wait             TYPE int4
                is_z2ui5_t_lock_req TYPE z2ui5_t_lock_req
                if_field_to_check   TYPE edpline
                if_field_set        TYPE abap_bool
      RETURNING VALUE(rs_info)      TYPE ty_s_tmstmp.

  PRIVATE SECTION.
    CLASS-METHODS condition_code
      IMPORTING if_user             TYPE uname
                is_z2ui5_t_lock_req TYPE z2ui5_t_lock_req
                if_field_to_check   TYPE edpline
                if_field_set        TYPE abap_bool
      RETURNING VALUE(rf_ok)        TYPE abap_bool.
ENDCLASS.


CLASS lcl_wait IMPLEMENTATION.
  METHOD wait.
    rs_info = VALUE ty_s_tmstmp( elapsed   = 0
                                 limit     = if_time
                                 condition = abap_false ).
    GET TIME STAMP FIELD rs_info-start.
    WHILE rs_info-elapsed < rs_info-limit.
      IF abap_true = condition_code( if_user = if_user
                                     if_field_to_check = if_field_to_check
                                     if_field_set = if_field_set
                                     is_z2ui5_t_lock_req = is_z2ui5_t_lock_req ).
        rs_info-condition = abap_true.
        EXIT.
      ENDIF.
      CALL FUNCTION 'ENQUE_SLEEP'
        EXPORTING
          seconds        = if_wait
        EXCEPTIONS
          system_failure = 1
          OTHERS         = 2.
      GET TIME STAMP FIELD rs_info-now.
      rs_info-elapsed = cl_abap_tstmp=>subtract( tstmp1 = rs_info-now
                                                 tstmp2 = rs_info-start ).
    ENDWHILE.
  ENDMETHOD.

  METHOD condition_code.
    DATA: lv_tabname TYPE tabnam VALUE 'Z2UI5_T_LOCK_REQ'.
    DATA: lr_struct_type TYPE REF TO cl_abap_structdescr,
          lr_itab_type   TYPE REF TO cl_abap_tabledescr,
          lt_comp        TYPE cl_abap_structdescr=>component_table,
          ls_comp        LIKE LINE OF lt_comp,
          lr_dref        TYPE REF TO data,
          lt_select      TYPE TABLE OF edpline.
    FIELD-SYMBOLS : <tab>   TYPE ANY TABLE,
                    <field> TYPE any.
    lr_struct_type ?= cl_abap_typedescr=>describe_by_name( lv_tabname ).
    lt_comp = lr_struct_type->get_components( ).
    lr_struct_type = cl_abap_structdescr=>create( lt_comp ).
    lr_itab_type   = cl_abap_tabledescr=>create( lr_struct_type ).
    CREATE DATA lr_dref TYPE HANDLE lr_itab_type.
    ASSIGN lr_dref->* TO <tab>.
    APPEND if_field_to_check TO lt_select.
    DATA(lt_where) = VALUE stringtab( ( |timestamp_request = '{ is_z2ui5_t_lock_req-timestamp_request }'| )
                                      ( |AND requester = '{ is_z2ui5_t_lock_req-requester }'| )
                                      ( |AND name_lock_object = '{ is_z2ui5_t_lock_req-name_lock_object }'| )
                                      ( |AND key_lock_object = '{ is_z2ui5_t_lock_req-key_lock_object }'| ) ).
    SELECT (lt_select) FROM (lv_tabname)
      INTO CORRESPONDING FIELDS OF TABLE <tab>
      WHERE (lt_where).
    IF sy-subrc = 0.
      rf_ok = abap_true.
      LOOP AT <tab> ASSIGNING FIELD-SYMBOL(<entry>).
        ASSIGN COMPONENT if_field_to_check OF STRUCTURE <entry> TO <field>.
        IF sy-subrc = 0 AND NOT <field> = if_field_set.
          rf_ok = abap_false.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
