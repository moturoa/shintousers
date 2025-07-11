#' @title MSAL User Management UI
#' @description A light version from our internal shintousers-app, so admin users can edit rights within the app
#' @param id ID of the module
#' @importFrom shiny NS column actionButton
#' @importFrom softui fluid_row box bsicon
#' @importFrom htmltools tags
#' @importFrom shinyjs hidden
#' @importFrom DT dataTableOutput
#' @export
msalUserManagementUI <- function(id){

  ns <- shiny::NS(id)

  softui::fluid_row(
    shiny::column(12,
                  htmltools::tags$div(id = ns("box_user"),
                                      softui::box(
                                        title = "Gebruikers",
                                        icon = bsicon("person"),
                                        width = 12,
                                        softui::fluid_row(style = "padding-top: 30px; padding-bottom: 30px;",
                                                          shiny::column(12,
                                                                        shinyjs::hidden(
                                                                          shiny::actionButton(ns("btn_remove_person"),
                                                                                              "Verwijderen",
                                                                                              icon = bsicon("person-dash"),
                                                                                              class = "btn-light")
                                                                        ),

                                                                        shinyjs::hidden(
                                                                          shiny::actionButton(ns("btn_edit_user"),
                                                                                              "Bewerken",
                                                                                              icon = bsicon("pencil-square"),
                                                                                              class = "btn-light")
                                                                        ),
                                                          )
                                        ),
                                        softui::fluid_row(
                                          shiny::column(12,
                                                 DT::dataTableOutput(ns("dt_roles"))
                                          )
                                        )
                                      )
                  )
    )
  )
}

#' @title MSAL User Management Module
#' @description A light version from our internal shintousers-app, so admin users can edit rights within the app
#' @param input input modalities of the module
#' @param output output modalities of the module
#' @param session R session object of the module
#' @param .user user object, connection to the shintousers database and functions necessary
#' @param appname appname for whoch the user can see connections
#' @importFrom gargoyle init trigger watch
#' @importFrom shiny reactive observe observeEvent showModal modalDialog tagList textInput radioButtons selectInput actionButton removeModal
#' @importFrom dplyr filter collect mutate slice
#' @importFrom DT renderDT
#' @importFrom shintoui datatafel
#' @importFrom shinyjs toggle disabled
#' @importFrom shinytoastr toastr_success
#' @importFrom softui bsicon
#' @export
msalUserManagementModule <- function(input, output, session, .user, appname){

  ns <- session$ns

  gargoyle::init("edit_role")

  app_data <- shiny::reactive({
    gargoyle::watch("edit_role")

    .user$read_table("shiny_msal_accounts", lazy = TRUE) %>%
      dplyr::filter(appname == !!appname) %>%
      dplyr::collect()
  })

  output$dt_roles <- DT::renderDT({

    app_data() %>%
      dplyr::mutate(active = ifelse(active,
                             as.character(bsicon("check-circle-fill", style = "color:green;")),
                             as.character(bsicon("dash-circle-fill", style = "color:red;"))
      )) %>%
      shintoui::datatafel(selection = "single", options = list(
        columnDefs = list(
          list(targets = 0, visible = FALSE)
        )
      ))
  })

  selected_persoon <- shiny::reactive({
    ii <- input$dt_roles_rows_selected
    if(is.null(ii))return(NULL)
    dplyr::slice(app_data(), ii)
  })


  # Persoon verwijderen
  shiny::observe({
    sel_p <- selected_persoon()
    shinyjs::toggle("btn_remove_person", condition = !is.null(sel_p))
    shinyjs::toggle("btn_edit_user", condition = !is.null(sel_p))
  })

  shiny::observeEvent(input$btn_remove_person, {

    .user$remove_role(userid = selected_persoon()$userid, appname = appname)
    shinytoastr::toastr_success("Persoon verwijderd")
    gargoyle::trigger("edit_role")

  })

  # Persoon bewerken
  shiny::observeEvent(input$btn_edit_user, {

    cur_role <- .user$get_role(selected_persoon()$userid, appname)
    cur_group <- .user$get_group(selected_persoon()$userid, appname)
    cur_name <- .user$get_name(selected_persoon()$userid, appname)
    cur_email <- .user$get_email(selected_persoon()$userid, appname)
    cur_comment <- .user$get_comment(selected_persoon()$userid, appname)
    cur_attribute <- .user$get_user_attributes(selected_persoon()$userid, appname)
    cur_active <- .user$get_user_active_inactive(selected_persoon()$userid, appname)



    shiny::showModal(
      shiny::modalDialog(
        fade = FALSE,
        easyClose = FALSE,
        title = shiny::tagList(softui::bsicon("pencil-square"), "Gebruiker bewerken"),
        shinyjs::disabled(
          shiny::textInput(ns("txt_userid_edit"), "Gebruiker id (rsconnect username)",
                    value = selected_persoon()$userid)
        ),
        shiny::textInput(ns("txt_name_edit"), "Gebruiker naam (label)",
                  value = cur_name),
        shiny::textInput(ns("txt_email_edit"), "Email adres",
                  value = cur_email),


        shiny::radioButtons(ns("rad_persoon_active"), "Actief",
                     inline = TRUE,
                     choices = c("Ja" = "active", "Nee" = "inactive"),
                     selected = ifelse(cur_active, "active", "inactive")),

        shiny::selectInput(ns("sel_role_edit"), "Rol",
                    choices = .user$get_application_roles(appname),
                    selected = cur_role
        ),

        shiny::selectInput(ns("sel_group_edit"), "Groepen",
                    choices = .user$get_application_groups(appname),
                    selected = cur_group, multiple = TRUE),

        shiny::textInput(ns("txt_comment_edit"), "Korte opmerking",
                  value = cur_comment),

        footer = shiny::tagList(
          shiny::actionButton("xyz", "Annuleren",
                       icon = icon("times"),
                       class= "btn-danger ontop",
                       `data-dismiss` = "modal"),
          shiny::actionButton(ns("btn_confirm_edit_user"),
                       "OK", icon = bsicon("check"),
                       class = "btn-success")
        )
      )
    )


  })

  shiny::observeEvent(input$btn_confirm_edit_user, {

    .user$set_user_name(userid = input$txt_userid_edit,
                        appname = appname,
                        username = input$txt_name_edit)
    .user$set_user_email(userid = input$txt_userid_edit,
                         appname = appname,
                         email = input$txt_email_edit)
    .user$set_role(userid = input$txt_userid_edit,
                   appname = appname,
                   role = input$sel_role_edit)
    .user$set_group(userid = input$txt_userid_edit,
                    appname = appname,
                    group = input$sel_group_edit)
    .user$set_comment(userid = input$txt_userid_edit,
                      appname = appname,
                      comments = input$txt_comment_edit)
    .user$set_user_attributes(userid = input$txt_userid_edit,
                              appname = appname,
                              attributes = NULL) #list(naam = input$txt_name)
    .user$set_user_active_inactive(userid = input$txt_userid_edit,
                                   appname = appname,
                                   what = input$rad_persoon_active)




    gargoyle::trigger("edit_role")
    shiny::removeModal()
  })


}

