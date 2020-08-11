$( document ).ready(function() {
  Shiny.addCustomMessageHandler('reset_form', function(id) {
    $("#" + id + " .form-control").val("");
    $("#" + id + " .shiny-file-input-progress").css({ visibility: "hidden" });
  });
});
