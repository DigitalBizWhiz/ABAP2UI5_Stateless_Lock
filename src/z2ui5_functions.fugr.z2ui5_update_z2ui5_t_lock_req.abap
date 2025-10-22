FUNCTION Z2UI5_UPDATE_Z2UI5_T_LOCK_REQ.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(IS_Z2UI5_T_LOCK_REQ) TYPE  Z2UI5_T_LOCK_REQ
*"----------------------------------------------------------------------
MODIFY z2ui5_t_lock_req FROM is_z2ui5_t_lock_req.
COMMIT WORK.



ENDFUNCTION.
