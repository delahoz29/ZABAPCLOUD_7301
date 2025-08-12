CLASS zcl_work_order_crud_test_7301 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .


    DATA: mo_handler TYPE REF TO zclworkorder_crud_handler_7301,
          out        TYPE REF TO object.



    METHODS:
      test_create_work_order IMPORTING io_out TYPE REF TO if_oo_adt_classrun_out,
      test_read_work_order   IMPORTING io_out TYPE REF TO if_oo_adt_classrun_out,
      test_update_work_order IMPORTING io_out TYPE REF TO if_oo_adt_classrun_out,
      test_delete_work_order IMPORTING io_out TYPE REF TO if_oo_adt_classrun_out,
      llenar_tablas_alternas.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.


CLASS zcl_work_order_crud_test_7301 IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.
    llenar_tablas_alternas(  ).
    test_create_work_order( out ).
*    test_read_work_order( out ).
*    test_update_work_order( out ).
*    test_delete_work_order( out ).


  ENDMETHOD.

  METHOD test_create_work_order.

    CREATE OBJECT mo_handler.

    " Datos para crear la orden de trabajo

    DATA(ls_workorder) = VALUE ztworkorder_7301(
      work_order_id  = '0000000003'
      customer_id    = 'Cli00003'
      technician_id  = 'TEC0003'
      creation_date  = cl_abap_context_info=>get_system_date( )
      status         = 'PE'
      priority       = 'A'
      description    = 'Mantenimiento de básculas.'
    ).

    DATA(lv_result) = mo_handler->create_work_order( iv_work_order_id = ls_workorder-work_order_id
                                                     iv_customer_id   = ls_workorder-customer_id
                                                     iv_technician_id = ls_workorder-technician_id
                                                     iv_priority      = ls_workorder-priority
                                                     iv_status        = ls_workorder-status
                                                     iv_description   = ls_workorder-description
                                                     iv_creation_date = ls_workorder-creation_date
                                                    ).


    IF lv_result = abap_true.
      io_out->write( |Registro creado satisfactoriamente con orden de trabajo N°: { ls_workorder-work_order_id } .| ).
    ELSE.
      io_out->write( |El registro de la orden de trabajo N°: { ls_workorder-work_order_id } no ha sido creado.| ).
    ENDIF.

  ENDMETHOD.

  METHOD test_read_work_order.
    CREATE OBJECT mo_handler.
    DATA:lv_work_order_id TYPE zde_work_order_id_7301 VALUE '0000000004',
         ls_work_order    TYPE ztworkorder_7301.

    DATA(lv_result) = mo_handler->read_work_order( EXPORTING iv_work_order_id = lv_work_order_id
                                                     IMPORTING ev_work_order = ls_work_order ).

    IF lv_result = abap_true.
      io_out->write( |Orden de trabajo encontrada! |
                  && |Orden de trabajo N°: { ls_work_order-work_order_id }. |
                  && |ID Cliente:          { ls_work_order-customer_id }. |
                  && |ID Técnico:          { ls_work_order-technician_id }. |
                  && |Fecha Creación:      { ls_work_order-creation_date }. |
                  && |Estado:              { ls_work_order-status }. |
                  && |Prioridad:           { ls_work_order-priority }. |
                  && |Descripción:         { ls_work_order-description }. |
       ).
    ELSE.
      io_out->write( |La orden de trabajo N°: { lv_work_order_id } no ha sido encontrada.| ).
    ENDIF.
  ENDMETHOD.

  METHOD test_update_work_order.
    CREATE OBJECT mo_handler.

    " Datos para actualizar la orden de trabajo

    DATA(ls_workorder) = VALUE ztworkorder_7301(
      work_order_id  = '0000000004'
      status         = 'PE'
      priority       = 'B'
      description    = 'Mantenimiento de caldera industrial Actualizado.'
    ).

    DATA(lv_result) = mo_handler->update_work_order( iv_work_order_id = ls_workorder-work_order_id
                                                     iv_priority      = ls_workorder-priority
                                                     iv_status        = ls_workorder-status
                                                    ).


    IF lv_result = abap_true.
      io_out->write( |Registro actualizado satisfactoriamente con orden de trabajo N°: { ls_workorder-work_order_id } .| ).
    ELSE.
      io_out->write( |El registro de la orden de trabajo N°: { ls_workorder-work_order_id } no ha sido actualizado.| ).
    ENDIF.
  ENDMETHOD.

  METHOD test_delete_work_order.
    CREATE OBJECT mo_handler.
    " Datos para crear la orden de trabajo

    DATA(ls_workorder) = VALUE ztworkorder_7301(
      work_order_id  = '0000000004'
    ).

    DATA(lv_result) = mo_handler->delete_work_order( iv_work_order_id = ls_workorder-work_order_id ).


    IF lv_result = abap_true.
      io_out->write( |Registro eliminado satisfactoriamente con orden de trabajo N°: { ls_workorder-work_order_id } .| ).
    ELSE.
      io_out->write( |El registro de la orden de trabajo N°: { ls_workorder-work_order_id } no ha sido eliminado.| ).
    ENDIF.
  ENDMETHOD.

  METHOD llenar_tablas_alternas.

    DATA lt_customers TYPE STANDARD TABLE OF ztcustomer_7301 WITH EMPTY KEY.

    lt_customers = VALUE #(
      ( customer_id = 'Cli00001' name = 'Ivan De la hoz'     address = 'Cra 5B N3 35'         phone = '3456878' )
      ( customer_id = 'Cli00002' name = 'Carla Jimenez'      address = 'Calle 45 N 23 78'     phone = '3623080' )
      ( customer_id = 'Cli00003' name = 'Romario Fernandez'  address = 'Km2 Via Funza'        phone = '3452789' )
      ( customer_id = 'Cli00004' name = 'Ledy Valencia'      address = 'Carrera 10 N4 38'     phone = '3451585' )
      ( customer_id = 'Cli00005' name = 'Valeria Sandoval'   address = 'Av. Ciudad Bolivar'   phone = '3665458' )
      ( customer_id = 'Cli00006' name = 'Yoimar Reyes'       address = 'Cra 34C N7 45'        phone = '3256969' )
      ( customer_id = 'Cli00007' name = 'Osvaldo Ospina'     address = 'Calle 5ta Diag. 34'   phone = '3136989' )
      ( customer_id = 'Cli00008' name = 'Fredy Fernandez'    address = 'Cra 5b N34 95'        phone = '3125478' )
      ( customer_id = 'Cli00009' name = 'Alex Tarazona'      address = 'Km 45 Via Cordalidad' phone = '3648798' )

    ).

    MODIFY ztcustomer_7301 FROM TABLE @lt_customers.

    DATA lt_technicians TYPE STANDARD TABLE OF zttechnician7301 WITH EMPTY KEY.

    lt_technicians = VALUE #(
      ( technician_id = 'TEC0001' name = 'William Gonzalez'      specialty = 'Soldador' )
      ( technician_id = 'TEC0002' name = 'Mauricio Rodriguez'    specialty = 'Eléctrico' )
      ( technician_id = 'TEC0003' name = 'Cesar Poblador'       specialty = 'Mecánico' )
      ( technician_id = 'TEC0004' name = 'Maria Fernandez'     specialty = 'Basculero' )
      ( technician_id = 'TEC0005' name = 'Patricia Acosta'     specialty = 'Fontanero' )
    ).

    MODIFY zttechnician7301 FROM TABLE @lt_technicians.

    DATA lt_priority TYPE STANDARD TABLE OF ztpriority_7301 WITH EMPTY KEY.

    lt_priority = VALUE #(
      ( priority_code = 'A' priority_description = 'High'     )
      ( priority_code = 'B' priority_description = 'Low'   )
    ).

    MODIFY ztpriority_7301 FROM TABLE @lt_priority.

    DATA lt_status TYPE STANDARD TABLE OF ztstatus_7301 WITH EMPTY KEY.

    lt_status = VALUE #(
      ( status_code = 'PE' status_description = 'Pending'     )
      ( status_code = 'CO' status_description = 'Completed'   )
    ).

    MODIFY ztstatus_7301 FROM TABLE @lt_status.

  ENDMETHOD.

ENDCLASS.
