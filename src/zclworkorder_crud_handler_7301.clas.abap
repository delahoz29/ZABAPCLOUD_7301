CLASS zclworkorder_crud_handler_7301 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
    ty_work_order      TYPE ztworkorder_7301.

    DATA: ls_work_order TYPE ztworkorder_7301.

    METHODS create_Work_order IMPORTING iv_work_order_id TYPE zde_work_order_id_7301
                                        iv_customer_id   TYPE zde_customer_id_7301
                                        iv_technician_id TYPE zde_technician_id_7301
                                        iv_priority      TYPE zde_priority_7301
                                        iv_status        TYPE zde_status_7301
                                        iv_description   TYPE zde_description_7301
                                        iv_creation_date TYPE zde_creation_date_7301

                              EXPORTING ev_validaciones  TYPE abap_bool

                              RETURNING VALUE(rv_result) TYPE abap_bool.

    METHODS read_work_order   IMPORTING iv_work_order_id     TYPE zde_work_order_id_7301

                              EXPORTING ev_work_order        TYPE ty_work_order

                              RETURNING VALUE(rv_work_order) TYPE abap_bool .


    METHODS update_work_order IMPORTING iv_work_order_id TYPE zde_work_order_id_7301
                                        iv_priority      TYPE zde_priority_7301
                                        iv_status        TYPE zde_status_7301

                              RETURNING VALUE(rv_result) TYPE abap_bool.


    METHODS delete_work_order IMPORTING iv_work_order_id TYPE zde_work_order_id_7301

                              RETURNING VALUE(rv_result) TYPE abap_bool.

    METHODS autority_check     IMPORTING iv_activity          TYPE c
                               RETURNING VALUE(rv_authorized) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: mo_validaciones TYPE REF TO zcl_validacion_work_order_7301.

ENDCLASS.


"
CLASS zclworkorder_crud_handler_7301 IMPLEMENTATION.

  METHOD autority_check.
    rv_authorized = abap_false.

    AUTHORITY-CHECK OBJECT 'ZAO_WO7301'
      ID 'ZAFWORKOR_' FIELD iv_activity.

    IF sy-subrc = 12.
      rv_authorized = abap_true.
    ELSE.
      rv_authorized = abap_false.
    ENDIF.

  ENDMETHOD.

  METHOD create_work_order.

    " Se valida si el usuario está autorizado para crear

    DATA(lv_authorized) = me->autority_check( iv_activity = '01' ).

    IF lv_authorized EQ abap_false.
      ev_validaciones = abap_false.
      RETURN.
    ENDIF.


    " Se aplican las validaciones para la creación de la orden de trabajo

    CREATE OBJECT mo_validaciones.

    DATA(lv_validaciones) = mo_validaciones->validate_create_order( iv_customer_id   = iv_customer_id
                                                                    iv_technician_id = iv_technician_id
                                                                    iv_priority      = iv_priority  ).

    IF lv_validaciones EQ abap_false.
      ev_validaciones = abap_false.
      RETURN.
    ENDIF.


    " Si cumple las validaciones se insertan los datos de la orden de trabajo
    ls_work_order = VALUE ztworkorder_7301( work_order_id  = iv_work_order_id
                                            customer_id    = iv_customer_id
                                            technician_id  = iv_technician_id
                                            creation_date  = iv_creation_date
                                            status         = iv_status
                                            priority       = iv_priority
                                            description    = iv_description
                                          ).

    INSERT ztworkorder_7301 FROM @ls_work_order.

    IF sy-subrc EQ 0.
      rv_result = abap_true.
    ELSE.
      rv_result = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD delete_work_order.

    CREATE OBJECT mo_validaciones.

    " Se valida si el usuario está autorizado para eliminar

    DATA(lv_authorized) = me->autority_check( iv_activity = '06' ).

    IF lv_authorized EQ abap_false.
      rv_result = abap_false.
      RETURN.
    ENDIF.

    "Se consulta el estado de la orden de trabajo

    SELECT SINGLE *
      FROM ztworkorder_7301
      WHERE work_order_id = @iv_work_order_id
      INTO @ls_work_order.

    IF sy-subrc NE 0.
      rv_result = abap_false.
      RETURN.
    ENDIF.

    " Se aplican las validaciones antes de la eliminación
    DATA(lv_validacion) = mo_validaciones->validate_delete_order(
      iv_work_order_id = iv_work_order_id
      iv_status        = ls_work_order-status ).

    IF lv_validacion = abap_false.
      rv_result = abap_false.
      RETURN.
    ENDIF.

    " Se elimina la orden de trabajo
    DELETE FROM ztworkorder_7301 WHERE work_order_id = @iv_work_order_id.

    IF sy-subrc EQ 0.
      rv_result = abap_true.
    ELSE.
      rv_result = abap_false.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD read_work_order.

    " Se valida si el usuario está autorizado para consultar

    DATA(lv_authorized) = me->autority_check( iv_activity = '03' ).

    IF lv_authorized EQ abap_false.
      rv_work_order = abap_false.
      RETURN.
    ENDIF.

    SELECT SINGLE *
      FROM ztworkorder_7301
      WHERE work_order_id = @iv_work_order_id
      INTO @ls_work_order.

    " Se valida si existe la orden de trabajo
    IF sy-subrc NE 0.
      CLEAR ls_work_order.
      rv_work_order = abap_false.
      RETURN.
    ENDIF.
    rv_work_order = abap_true.
    ev_work_order = ls_work_order.
  ENDMETHOD.

  METHOD update_work_order.
    CREATE OBJECT mo_validaciones.

    " Se valida si el usuario está autorizado para modificar

    DATA(lv_authorized) = me->autority_check( iv_activity = '02' ).

    IF lv_authorized EQ abap_false.
      rv_result = abap_false.
      RETURN.
    ENDIF.

    " Se valida antes de actualziar la orden de trabajo
    DATA(lv_validacion) = mo_validaciones->validate_update_order(
      iv_work_order_id = iv_work_order_id
      iv_status        = iv_status

    ).

    IF lv_validacion EQ abap_false.
      rv_result = abap_false.
      RETURN.
    ENDIF.

    " Se actualiza la orden de trabajo
    UPDATE ztworkorder_7301 SET
           status        = @iv_status,
           priority      = @iv_priority
      WHERE work_order_id = @iv_work_order_id.

    IF sy-subrc EQ 0.

      " Se insertan los datos en el historial
      DATA(ls_workorhist)  = VALUE ztworkorhist7301(    history_id         = cl_abap_context_info=>get_system_date(  ) && cl_abap_context_info=>get_system_time( )  "Se agrega la fecha + la hora
                                                        work_order_id      = iv_work_order_id
                                                        modification_date  = cl_abap_context_info=>get_system_date( )
                                                        change_description = |Se actualzia el estado  { iv_status } y la prioridad  { iv_priority }| ).

      INSERT ztworkorhist7301 FROM @ls_workorhist.


      rv_result = abap_true.
    ELSE.
      rv_result = abap_false.
      RETURN.
    ENDIF.
  ENDMETHOD.



ENDCLASS.
