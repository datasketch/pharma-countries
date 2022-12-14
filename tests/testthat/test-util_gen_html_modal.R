test_that("Modal works", {
  a= "<button id='Uk paper'>i</button> <div id='Uk paper_Uk paper' class='modal' <div class='modal-content'> <span class='close'>&times;</span> <p> <p>Some text in the Modal..</p> </p> </div> </div>"
  var_test = "<p>Some text in the Modal..</p>"
  id="Uk paper"

  expect_equal(gen_html_detail_to_modal(id,var_test) , a)
})
