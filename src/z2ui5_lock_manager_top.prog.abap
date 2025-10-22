*&---------------------------------------------------------------------*
*& Include Z2UI5_LOCK_MANAGER_TOP
*&---------------------------------------------------------------------*

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
TYPES: BEGIN OF ts_parameter_helper,
         name  TYPE fieldname,
         value TYPE fieldvalue,
       END OF ts_parameter_helper.
TYPES tt_parameter_helper TYPE HASHED TABLE OF ts_parameter_helper WITH UNIQUE KEY name.
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
TYPES: BEGIN OF ty_range,
         sign   TYPE c LENGTH 1,
         option TYPE c LENGTH 2,
         low    TYPE enqu_name,
       END OF ty_range.
TYPES tt_z2ui5_t_lock_req TYPE STANDARD TABLE OF z2ui5_t_lock_req WITH DEFAULT KEY.
TYPES: BEGIN OF ts_lock_data,
         t_table_mode       TYPE tt_table_mode,
         t_parameter        TYPE tt_parameter_helper,
         v_lock_object_name TYPE enqu_name,
       END OF ts_lock_data.
