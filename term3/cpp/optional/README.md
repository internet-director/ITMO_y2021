# Optional

В этом задании вам необходимо написать [`optional`](https://en.cppreference.com/w/cpp/utility/optional/optional)  
Интерфейс вы найдете в репозитории, в файле `optional.h`. Поведение должно соответствовать `std::optional`.

Обратите внимание, что должно выполняться:
- Если у типа `T` нет какого-то из special members (например, `is_copy_constructible<T> == false`), то это свойство должно сохраняться и для `optional<T>`.
- Если тип `T` удовлетворяет какому-то из трейтов `is_trivially_*`, то и `optional<T>` должен ему удовлетворять.
- `optional` может использоваться в constexpr контексте, см. тесты со `static_assert`. Исключение - для нетривиально присваеваемых классов не требуется реализация `constexpr operator=` и constexpr конструктора копирования. 

