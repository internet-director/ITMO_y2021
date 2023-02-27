# intrusive-ptr

Вам нужно реализовать 
[intrusive_ptr](https://www.boost.org/doc/libs/1_71_0/libs/smart_ptr/doc/html/smart_ptr.html#intrusive_ptr) 
и [intrusive_ref_counter](https://www.boost.org/doc/libs/1_71_0/libs/smart_ptr/doc/html/smart_ptr.html#intrusive_ref_counter), 
а также некоторые вспомогательные утилиты (операторы, API счётчика).

Обратите внимание, что `intrusive_ref_counter` должен иметь единственную политику потокобезопасности, 
аналогичную (и не уступающую в эффективности) `boost::thread_safe_counter`. 

Будьте готовы ответить на вопросы о выбранном `memory_order`.
