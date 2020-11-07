function make_slides(f) {
  var slides = {};

  slides.bot = slide({
    name: "bot",
    start: function () {
      $('.err1').hide();
      $('.err2').hide();
      $('.disq').hide();
      exp.speaker = _.shuffle(["James", "John", "Robert", "Michael", "William", "David", "Richard", "Joseph", "Thomas", "Charles"])[0];
      exp.listener = _.shuffle(["Mary", "Patricia", "Jennifer", "Linda", "Elizabeth", "Barbara", "Susan", "Jessica", "Sarah", "Margaret"])[0];
      exp.lives = 0;
      var story = exp.speaker + ' says to ' + exp.listener + ': "It\'s a beautiful day, isn\'t it?"'
      var question = 'Who does ' + exp.speaker + ' talk to?';
      document.getElementById("s").innerHTML = story;
      document.getElementById("q").innerHTML = question;
    },
    button: function () {
      exp.text_input = document.getElementById("text_box").value;
      var lower = exp.listener.toLowerCase();
      var upper = exp.listener.toUpperCase();

      if ((exp.lives < 3) && ((exp.text_input == exp.listener) | (exp.text_input == lower) | (exp.text_input == upper))) {
        exp.data_trials.push({
          "slide_number_in_experiment": exp.phase,
          "tgrep_id": "bot_check",
          "response": [exp.text_input, exp.listener],
          "sentence": "",
        });
        exp.go();
      }
      else {
        exp.data_trials.push({
          "slide_number_in_experiment": exp.phase,
          "tgrep_id": "bot_check",
          "response": [exp.text_input, exp.listener],
          "sentence": "",
        });
        if (exp.lives == 0) {
          $('.err1').show();
        } if (exp.lives == 1) {
          $('.err1').hide();
          $('.err2').show();
        } if (exp.lives == 2) {
          $('.err2').hide();
          $('.disq').show();
          $('.button').hide();
        }
        exp.lives++;
      }
    },
  });

  slides.i0 = slide({
    name: "i0",
    start: function () {
      $("#n_trials").html(exp.n_trials);
      exp.startT = Date.now();
    }
  });

  slides.example1 = slide({
    name: "example1",

    start: function () {
      $('.err').hide();

      var contexthtml = "<b>Speaker #1</b>: The party last night was packed! And there was lots of drama. <br> <b>Speaker #2</b>: I wish I could have gone, but I had to study. "
      var entirehtml = "<font color=#FF0000> " + "Who was there?"
      contexthtml = contexthtml + entirehtml

      exp.aParaphrase.value = '<label><input type="radio" name="paraphrase" value="a"/>' + "Who is a person that was at the party?" + '</label>'
      exp.theParaphrase.value = '<label><input type="radio" name="paraphrase" value="the"/>' + "Who is the person that was at the party?" + '</label>'
      exp.someParaphrase.value = '<label><input type="radio" name="paraphrase" value="some"/>' + "Who is some person that was at the party?" + '</label>'
      exp.allParaphrase.value = '<label><input type="radio" name="paraphrase" value="all"/>' + "Who is every person that was at the party?" + '</label>'

      $(".context").html(contexthtml);

      // console.log("exp.paraphraseArray: ", exp.paraphraseArray)

      for (i=0; i< 4; i++){
        $(`.loc${i+1}`).html(exp.paraphraseArray[i].value)
      }
      $(".err").hide();
    },

    button: function () {
      this.radio = $("input[name='paraphrase']:checked").val();
      console.log(this.radio)
      if (this.radio == "all") {
        this.log_responses();
        exp.go();
      }
      else {
        $('.err').show();
        this.log_responses();
      }
    },

    log_responses: function () {
      exp.data_trials.push({
        "slide_number_in_experiment": exp.phase,
        "tgrep_id": "example1",
        "response": [this.radio, ""],
        "sentence": "",
      });
    },
  });


  slides.example2 = slide({
    name: "example2",

    start: function () {
      $(".err").hide();

      var contexthtml = "<b>Speaker #1</b>: Excuse me, could you help me please? I'm not from around here. <br> <b>Speaker #2</b>: Sure, how can I help?<br> <b>Speaker #1</b>: "
      var entirehtml = "<font color=#FF0000> " + "Where can I get coffee?"
      contexthtml = contexthtml + entirehtml
      
      exp.theParaphrase.value = '<label><input type="radio" name="paraphrase" value="the"/>' + "What is the place that I get get coffee?" + '</label>'
      exp.aParaphrase.value = '<label><input type="radio" name="paraphrase" value="a"/>' + "What is a place that I can get coffee?" + '</label>'
      exp.someParaphrase.value = '<label><input type="radio" name="paraphrase" value="some"/>' + "What is some place that I can get coffee?" + '</label>'
      exp.allParaphrase.value = '<label><input type="radio" name="paraphrase" value="all"/>' + "What is every place that I can get coffee?" + '</label>'

      $(".context").html(contexthtml);

      for (i=0; i< 4; i++){
        $(`.loc${i+1}`).html(exp.paraphraseArray[i].value)
      }
      $(".err").hide();

    },
    button: function () {
      this.radio = $("input[name='number']:checked").val();
      if (this.radio == "a" | this.radio == "some" | this.radio == "the") {
        this.log_responses();
        exp.go();
      }
      else {
        $('.err').show();
        this.log_responses();
      }
    },

    log_responses: function () {
      exp.data_trials.push({
        "slide_number_in_experiment": exp.phase,
        "tgrep_id": "example2",
        "response": [this.radio, ""],
        "sentence": "",
      });
    },
  });

  slides.startExp = slide({
    name: "startExp",
    start: function () {
      $("#instrunctionGen").html('<table class=table1 id="instructionGen"> </table>');
      var dispRow = $(document.createElement('tr')).attr("id", 'rowp' + 1);
      dispRow.append("<div class=row>");
      dispRow.append("<div align=center><button class=continueButton onclick= _s.button()>Continue</button></div>");
      dispRow.append('<td/>');
      dispRow.append('</div>');
      dispRow.appendTo("#instructionGen");

    },
    button: function () {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    },
  });

  slides.generateEntities = slide({
    name: "generateEntities",
    present: exp.stimuli, // This the array generated from stimuli.js

    present_handle: function (stim) { // this function is called bascially on exp.stim (more or less)

      $("input[name='number']:checked").prop('checked', false);

      this.counter = 1;

      var generic = stim;
      this.generic = generic;
      this.response = response = false;

      // var contexthtml = generic.PreceedingContext;
      var contexthtml = this.format_context(generic.PreceedingContext);
      var entirehtml = "<font color=#FF0000> " + this.format_sentence(generic.EntireSentence)
      contexthtml = contexthtml + entirehtml

      // This is where the dependent variable is
      exp.theParaphrase.value = '<label><input type="radio" name="paraphrase" value="the"/>' + generic.TheResponse + '</label>'
      exp.aParaphrase.value = '<label><input type="radio" name="paraphrase" value="a"/>' + generic.AResponse + '</label>'
      exp.someParaphrase.value = '<label><input type="radio" name="paraphrase" value="some"/>' + generic.SomeResponse + '</label>'
      exp.allParaphrase.value = '<label><input type="radio" name="paraphrase" value="all"/>' + generic.AllResponse + '</label>'
      
      for (i=0; i<4; i++){
        $(`.loc${i+1}`).html(exp.paraphraseArray[i].value)  
      }

      $(".context").html(contexthtml);

      $(".err").hide();

      this.counter++;

      var checkbox = document.createElement('input');
      checkbox.setAttribute('align', 'center');
      exp.checkbox = document.getElementById("strange");

      $(".err").hide();

    },

    // speakers 1 and 2 
    format_context: function (context) {
      // remove all ### standing alone
      contexthtml = context.replace(/###/g, " ");
      // replace first three ## with Speaker 1
      contexthtml = contexthtml.replace(/speakera(\d+)./g, "<br><b>Speaker #1: </b>");
      contexthtml = contexthtml.replace(/speakerb(\d+)./g, "<br><b>Speaker #2: </b>");
      // remove the traces
      contexthtml = contexthtml.replace(/\*t*\**\-(\d+)/g, "");
      // remove random asterisks
      contexthtml = contexthtml.replace(/\*/g, "");


      // this just deals with the first instance of speaker
      if (!contexthtml.startsWith("<br><b>Speaker #")) {
        var ssi = contexthtml.indexOf("Speaker #");
        switch (contexthtml[ssi + "Speaker #".length]) {
          case "1":
            contexthtml = "<br><b>Speaker #2:</b> " + contexthtml;
            break;
          case "2":
            contexthtml = "<br><b>Speaker #1:</b> " + contexthtml;
            break;
          default:
            break;
        }
      };
      return contexthtml;
    },

    format_sentence: function (sentence) {
      // remove the traces
      entirehtml = sentence.replace(/\*t*\**\-(\d+)/g, "");
    },

    button: function () {
      this.radio = $("input[name='paraphrase']:checked").val(); // take the value of the paraphrase-named button that's checked
      if (this.radio) {
        this.log_responses();
        $(this.radio).prop('checked', false);
        exp.checkbox.checked = false;
        //this.checked = false;
        _stream.apply(this); //go to the next element
      }
      else {
        $('.err').show();
        //this.log_responses();
      }
    },

    log_responses: function () {
      exp.data_trials.push({
        "slide_number_in_experiment": exp.phase,
        "tgrep_id": this.generic.TGrepID,
        "response": [this.radio, exp.checkbox.checked],
        // "sentence": this.generic.BestResponse,
      });
    },

  });

  slides.subj_info = slide({
    name: "subj_info",
    submit: function (e) {
      //ifFdata (e.preventDefault) e.preventDefault(); // I don't know what this means.
      exp.subj_data = _.extend({
        language: $("#language").val(),
        enjoyment: $("#enjoyment").val(),
        asses: $('input[name="assess"]:checked').val(),
        age: $("#age").val(),
        gender: $("#gender").val(),
        education: $("#education").val(),
        problems: $("#problems").val(),
        fairprice: $("#fairprice").val(),
        comments: $("#comments").val(),
        paraArray: [exp.paraphraseArray[0].name,exp.paraphraseArray[1].name,exp.paraphraseArray[2].name,exp.paraphraseArray[3].name]
      });
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.thanks = slide({
    name: "thanks",
    start: function () {
      exp.data = {
        "trials": exp.data_trials,
        "catch_trials": exp.catch_trials,
        "system": exp.system,
        "condition": exp.condition,
        "subject_information": exp.subj_data,
        "time_in_minutes": (Date.now() - exp.startT) / 60000
      };
      setTimeout(function () { proliferate.submit(exp.data); }, 1000);
    }
  });

  return slides;
}

/// init ///
function init() {

  repeatWorker = false;
  //exp.n_entities = 1;
  exp.names = [];
  exp.all_names = [];
  exp.trials = [];
  exp.catch_trials = [];
  var stimuli = generate_stim(); // this calls a function in stimuli.js
  exp.theParaphrase={name:"theParaphrase"};
  exp.aParaphrase={name:"aParaphrase"};
  exp.allParaphrase={name:"allParaphrase"};
  exp.someParaphrase={name:"someParaphrase"};
  exp.paraphraseArray = _.shuffle([exp.theParaphrase,exp.aParaphrase,exp.allParaphrase,exp.someParaphrase])
  console.log(stimuli.length);
  //exp.stimuli = _.shuffle(stimuli).slice(0, 15);
  exp.stimuli = stimuli.slice();
  exp.stimuli = _.shuffle(exp.stimuli);
  exp.n_trials = exp.stimuli.length;
  exp.stimcounter = 0;

  exp.stimscopy = exp.stimuli.slice();

  exp.system = {
    Browser: BrowserDetect.browser,
    OS: BrowserDetect.OS,
    screenH: screen.height,
    screenUH: exp.height,
    screenW: screen.width,
    screenUW: exp.width
  };
  //blocks of the experiment:
  exp.structure = [
    "bot",
    "i0",
    "example1",
    "example2",
    "startExp",
    "generateEntities", // This is where the test trials come in.
    //"priors",
    "subj_info",
    "thanks"
  ];

  exp.data_trials = [];
  //make corresponding slides:
  exp.slides = make_slides(exp);

  exp.nQs = utils.get_exp_length(); //this does not work if there are stacks of stims (but does work for an experiment with this structure)
  //relies on structure and slides being defined

  $('.slide').hide(); //hide everything

  //make sure turkers have accepted HIT (or you're not in mturk)
  $("#start_button").click(function () {
    if (turk.previewMode) {
      $("#mustaccept").show();
    } else {
      $("#start_button").click(function () { $("#mustaccept").show(); });
      exp.go();
    }
  });

  exp.go(); //show first slide
}
