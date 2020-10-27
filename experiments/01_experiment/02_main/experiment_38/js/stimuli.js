
var path = 'https://raw.githubusercontent.com/thegricean/corpus_implicatures/master/or/experiment/3_implicature_exp/better_bnb_korpus/korpus3_38.txt'
console.log(path);
 
function get_data(fullpath) {
    var response = $.ajax({
        type: "GET",
        async: false,
        url: fullpath,
        dataType: "text",
    });
    return response.responseText;
}
 
function generate_stim() {
   
    var contents = get_data(path);
    var raw = contents;
        var rows = raw.split('\n');
 
        var data = [];
        headings = rows[0].split('\t');
        var total = rows.length - 1;

        headings = ["TGrep","EntireSentence", "context", "BestResponse"]
        //old headings TGrepID EntireSentence context BestReponse
        //var total = rows.length; //CHANGED
   	     
        for (var i = 1; i < total; i++) {		
                data.push(rows[i].split('\t'));
            } 

        // console.log("data: ",data);
 
        var stim = data.map(function(row) {
            return row.reduce(function(result, field, index) {
                result[headings[index]] = field;
                return result;
            }, {});
        });

        // console.log("stim: ",stim);
   
    return stim;
}
 