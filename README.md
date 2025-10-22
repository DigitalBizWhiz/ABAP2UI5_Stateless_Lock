# ABAP2UI5_Stateless_Lock
SAP resources can be locked and unlocked from stateless ABAP2UI5 Apps. This feature fully handles the standard ENQUEUE and DEQUEUE process by using a background job that runs only when necessary.

      DATA(lo_class) = NEW z2ui5_cl_lock_request( it_table_mode = VALUE #( ( table_name = 'LIKP' mode = 'O' ) )
                                                  if_lock_object = 'EVVBLKE'
                                                  it_lock_parameter = VALUE #( ( name = 'VBELN' value = '0086900051' ) ) ).
      lo_class->enqueue( if_username_job = 'SAP_USERNAME' ).
      lo_class->get_lock( ).
