//$(document).ajaxStart($.blockUI).ajaxStop($.unblockUI);

$.blockUI.defaults.baseZ = 2000;
$.blockUI.defaults.message = '<div class="row><div class="col-xs-2"><p>Por favor, aguarde...</div></div>', 

$(document).ready(function () {
    
});

function FormatDecimal(value, decimals) {
    var number = Number(Math.round(value+'e'+decimals)+'e-'+decimals);
    var qtdCasas = number.toString().split('.')[1];
    
    if (qtdCasas == null || qtdCasas == undefined || qtdCasas.length == 0) {
        return number + '.00'
    }
    
    if (qtdCasas.length == 1) {
        return number + '0'
    }
    
    return number;
}