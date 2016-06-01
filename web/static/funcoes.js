/* global $*/
//$(document).ajaxStart($.blockUI).ajaxStop($.unblockUI);

$.blockUI.defaults.baseZ = 2000;
$.blockUI.defaults.message = '<div class="row><div class="col-xs-2"><p>Por favor, aguarde...</div></div>', 

$(document).ready(function () {
    TrataValorMonetario();
});

var TrataValorMonetario = function () {
    
    $('span.valorMonetario').each(function () {
        var valor = $(this).html();
        
        valor = FormatDecimal(valor, 2);
        
        $(this).html(valor);
        $(this).attr('data-mask', '#####0,00');
        $(this).attr('data-mask-reverse', 'true');
    });
    
    $('label.valorMonetario').each(function () {
        var valor = $(this).html();
        
        valor = FormatDecimal(valor, 2);
        
        $(this).html(valor);
        $(this).attr('data-mask', '#####0,00');
        $(this).attr('data-mask-reverse', 'true');
    });
    
    $('input.valorMonetario').each(function () {
        var valor = $(this).val();
        
        valor = FormatDecimal(valor, 2);
        
        $(this).val(valor);
        $(this).attr('data-mask', '#####0,00');
        $(this).attr('data-mask-reverse', 'true');
    });
}

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