# auction

Postman - https://www.getpostman.com/collections/2916edd248e61ff7bdbc

Вопросы:
1. В ImplLotSessionStore дубликат кода из ImplSimpleStore, т.к. Ref оказался инвариантным и не удается переиспользовать код с разными State. Как обойти ?
2. В LotSessionStore для запуска F[_] добавил Unsafe (больше ничего не придумал), это корректное ршение ? И есть лт другие варианты ? 
