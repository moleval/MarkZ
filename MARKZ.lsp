;;;=====================================================================
;;;  MARKZ.lsp  —  ЗАПОЛНЕНИЕ АТРИБУТОВ МАРКИ ЗАПОЛНЕНИЙ
;;;  Файл: D:\MARKZ.lsp
;;;  Команды: МАРКА, МАРКАРОВКА, МАРКАРЯД, МАРКАТАБЛ, МАРКАБЛОК
;;;
;;;  Режим самодиагностики TEST 00 ... TEST 14.
;;;  TEST 00 — прогон скобок исходника MARKZ.lsp.
;;;  Этап A: выбор > чтение > диагностика > расчёт > PRE-CHECK
;;;  Этап B: запись атрибута "Марка" только после PRE-CHECK = OK
;;;
;;;  Загрузка:
;;;    (load "D:/MARKZ.lsp")
;;;    МАРКАРОВКА
;;;
;;;  ВАЖНО: имена блоков, атрибутов и Dynamic Properties должны
;;;  точно совпадать с чертежом. Если Visibility называется иначе —
;;;  добавьте имя в *mark:prop-vis-candidates* (см. TEST 07).
;;;=====================================================================

(vl-load-com)

;;;--------------------- Шапка: имена и допуски (править здесь) ------

;; Точное имя или маска wcmatch, напр.: "*аполнение*витраж*"
(setq *mark:block-fill*    "Заполнение в витраж")
(setq *mark:block-glazing* "Атрибуты витража")
(setq *mark:attr-vitrage*  "Витраж")
(setq *mark:attr-mark*     "Марка")
(setq *mark:prop-width*    "Ширина в свету")
(setq *mark:prop-height*   "Высота в свету")

;; Кандидаты имени Dynamic Property состояния Visibility
(setq *mark:prop-vis-candidates*
  '("Видимость1"
    "Видимость"
    "Visibility"
    "Visibility1"
    "Состояние видимости"
    "Visibility State"
    "Вид"
    "Видимость2"))

;; 29 букв: нет Ё, Й, З, О. После Я — АА, АБ, … БА, ББ. Не останавливаемся.
(setq *mark:letters*
  '("А" "Б" "В" "Г" "Д" "Е" "Ж" "И"
    "К" "Л" "М" "Н" "П" "Р" "С" "Т" "У" "Ф"
    "Х" "Ц" "Ч" "Ш" "Щ" "Ъ" "Ы" "Ь" "Э" "Ю" "Я"))

(defun mark:letter-at (n / base len span start off digits d ch)
  (setq base (length *mark:letters*))
  (cond
    ((or (null n) (not (numberp n)) (< n 0) (<= base 0))
     nil)
    ((< n base)
     (nth (fix n) *mark:letters*))
    (t
     (setq len   1
           span  base
           start 0)
     (while (and (< len 4) (>= n (+ start span)))
       (setq start (+ start span)
             len   (1+ len)
             span  (* span base)))
     (setq off    (fix (- n start))
           digits nil)
     (repeat len
       (setq d      (rem off base)
             off    (/ off base)
             ch     (nth d *mark:letters*)
             digits (cons (if ch ch "") digits)))
     (apply 'strcat digits))))

;; Специальные Visibility > приписка (без дефиса, нижний регистр)
(setq *mark:specials*
  '(("Стемалит" . "стм")
    ("Сэндвич" . "снд")))
;; Сколько блоков показывать в TEST 13
(setq *mark:test13-show*  10)
;; Сколько строк ошибок показывать в одном тесте (остальное — сводка)
(setq *mark:max-err-lines* 10)

;; Рядовка
(setq *mark:ar-block*   "Ряд заполнений")
(setq *mark:ar-attr*    "Ряд")
(setq *mark:ar-group-h* "Рядовка горизонтальная")
(setq *mark:ar-group-v* "Рядовка вертикальная")
(setq *mark:ar-offset*  1000.0)
(setq *mark:ar-gap*     1000.0)
(setq *mark:ar-line*    150.0)   ; шаг линий 1+, мм
(setq *mark:ar-tol*     1.0)

;; Сетка заполнений
(setq *mark:fill-tol*    30.0)    ; мм — выравнивание осей
(setq *mark:fill-slope*  0.0033)  ; уклон стойки, не ослаблять без чертежа
(setq *mark:fill-window* 5000.0)  ; мм — окно поиска для режима «Точка»
(setq *mark:fill-max-w*  3000.0)  ; шире — не ячейка, а разрыв между участками
(setq *mark:fill-max-h*  4500.0)

;; Ведомость
(setq *mtab:allowance* 26)
(setq *mtab:h-keys* '("Высота в свету" "ВЫСОТА В СВЕТУ" "ВЫСОТА" "Height"))
(setq *mtab:w-keys* '("Ширина в свету" "ШИРИНА В СВЕТУ" "ШИРИНА" "ДЛИНА" "Width"))

;;;--------------------- Состояние сеанса -----------------------------

;; Редакция модуля — видно в консоли при загрузке и в баннерах
(setq *mark:rev*    "Ред. 42")

;; МАРКА: один выбор; один UNDO на весь пакет
(setq *mark:reuse-sel* nil)
(setq *mark:batch-undo* nil)
(setq *mark:seg-cache* nil)
(setq *mark:udef-cache* nil)
(setq *mark:mline-cache* nil)
(setq *mark:no-expl-undo* nil)
(setq *mark:expl-n* 0)
(setq *mark:def-n* 0)

(defun mark:reset-state ()
  (setq *mark:dyn-cache*    nil
        *mark:seg-cache*    nil
        *mark:no-expl-undo* nil
        *mark:errors*       0
        *mark:warnings*     0
        *mark:records*      nil
        *mark:cnt-stem*     0
        *mark:cnt-sand*     0
        *mark:written*      0
        *mark:precheck-ok*  nil
        *mark:apply-failed* 0)
  ;; выбор и таблицы индексов не трогаем, если МАРКА переиспользует
  (if (not *mark:reuse-sel*)
    (setq *mark:found-names*   nil
          *mark:sel-total*     0
          *mark:others*        0
          *mark:fills*         nil
          *mark:glazings*      nil
          *mark:prefix*        ""
          *mark:prefix-found*  nil
          *mark:widths*        nil
          *mark:heights*       nil)))

(mark:reset-state)

;;;--------------------- Сообщения ------------------------------------

(defun mark:out (msg)
  (prompt (strcat msg "\n")))

;; Одна строка в командной строке при старте. В пакете МАРКА не повторяем.
(defun mark:cmd-line (text)
  (if (not *mark:reuse-sel*)
    (prompt (strcat "\n" text "\n"))))

(defun mark:note-error ()
  (setq *mark:errors* (1+ *mark:errors*)))

(defun mark:note-warning ()
  (setq *mark:warnings* (1+ *mark:warnings*)))

(defun mark:banner ()
  (mark:out "========================================")
  (mark:out (strcat " ЗАПОЛНЕНИЕ АТРИБУТОВ МАРКИ  "
                    *mark:rev*))
  (mark:out "========================================"))

;;;--------------------- Утилиты --------------------------------------

;; Переносимая проверка строки (не зависит от `stringp` из Visual LISP)
(defun mark:strp (x)
  (and x (eq (type x) 'STR)))

(defun mark:trim (s)
  (if (mark:strp s)
    (vl-string-trim " \t\r\n" s)
    s))

(defun mark:name= (s1 s2)
  (and (mark:strp s1)
       (mark:strp s2)
       (= (strcase (mark:trim s1))
          (strcase (mark:trim s2)))))

(defun mark:fmt-raw (v / u)
  (setq u (mark:unwrap v))
  (if (or (numberp u) (mark:strp u))
    (setq v u))
  (cond
    ((null v) "NIL")
    ((numberp v)
     (if (= (float v) (fix (float v)))
       (itoa (fix (float v)))
       (rtos (float v) 2 8)))
    ((mark:strp v) (strcat "\"" v "\""))
    (t (vl-princ-to-string (mark:unwrap v)))))

(defun mark:numval (v / u)
  (setq u (mark:unwrap v))
  (cond
    ((numberp u) (float u))
    ((numberp v) (float v))
    ((and (mark:strp u) (distof u 2)) (distof u 2))
    ((and (mark:strp v) (distof v 2)) (distof v 2))
    (t nil)))

(defun mark:pad-line (label value)
  (strcat " "
          label
          (substr "                                  " 1
                  (max 1 (- 36 (strlen label))))
          value))

;; Печать списка строк с ограничением *mark:max-err-lines*
(defun mark:print-limited (lines / total shown)
  (setq total (length lines)
        shown 0)
  (foreach ln lines
    (if (< shown *mark:max-err-lines*)
      (progn
        (setq shown (1+ shown))
        (mark:out ln))))
  (if (> total *mark:max-err-lines*)
    (mark:out (strcat "... и ещё "
                      (itoa (- total *mark:max-err-lines*))
                      " строк(и)."))))

(defun mark:insert-sorted (lst n / res done hit)
  (setq res nil
        done nil
        hit   nil)
  (foreach x lst
    (if (mark:same-num? n x)
      (setq hit t))
    (if (and (null done)
             (null hit)
             (< n x))
      (progn
        (setq res (cons n res))
        (setq done t)))
    (setq res (cons x res)))
  (if (and (not done) (not hit))
    (setq res (cons n res)))
  (reverse res))

(defun mark:same-num? (a b)
  ;; одинаковый размер: 0.5 мм — шум float/округление, не «другой типоразмер»
  (and (numberp a) (numberp b) (equal a b 0.5)))

;; округление размера для ключа индекса (0.5 мм)
(defun mark:dim-key (x)
  (if (and (numberp x) (> x 0.0))
    (fix (+ (* x 2.0) 0.5))  ; half-mm integer key
    nil))

(defun mark:sort-unique (nums / res n hit x)
  (setq res nil)
  (foreach n nums
    (setq hit nil)
    (foreach x res
      (if (and (null hit) (mark:same-num? x n))
        (setq hit t)))
    (if (not hit)
      (setq res (mark:insert-sorted res n))))
  res)

(defun mark:positives (lst / out)
  (setq out nil)
  (foreach n lst
    (if (and (numberp n) (> n 0.0))
      (setq out (cons (float n) out))))
  (reverse out))

(defun mark:index-of (lst v / i found)
  (setq i 0
        found nil)
  (foreach x lst
    (if (and (null found) (mark:same-num? x v))
      (setq found i))
    (setq i (1+ i)))
  found)

(defun mark:rec-get (r key)
  (cdr (assoc key r)))

(defun mark:rec-put (r key val)
  (if (assoc key r)
    (subst (cons key val) (assoc key r) r)
    (append r (list (cons key val)))))

(defun mark:vla (e)
  (vlax-ename->vla-object e))

;; Имя блока: DXF-код 2 + имя через ActiveX (для динамических блоков)
;; Безопасное чтение ActiveX-свойства через vlax-get-property
(defun mark:ax-get (obj prop / r)
  (setq r (vl-catch-all-apply 'vlax-get-property (list obj prop)))
  (if (vl-catch-all-error-p r)
    nil
    r))

;; Безопасная запись ActiveX-свойства
(defun mark:ax-put (obj prop val / r)
  (setq r (vl-catch-all-apply 'vlax-put-property (list obj prop val)))
  (if (vl-catch-all-error-p r)
    nil
    t))

;; Вызов ActiveX-метода (аргументы после method)
(defun mark:ax-invoke (obj method arg / r)
  (setq r (vl-catch-all-apply 'vlax-invoke-method
                              (if arg
                                (list obj method arg)
                                (list obj method))))
  (if (vl-catch-all-error-p r)
    nil
    r))

;; T если метод вызван без ошибки (void-методы возвращают nil — это OK)
(defun mark:ax-invoke-ok (obj method arg / r)
  (setq r (vl-catch-all-apply 'vlax-invoke-method
                              (if arg
                                (list obj method arg)
                                (list obj method))))
  (not (vl-catch-all-error-p r)))

;; Все известные имена вставки: DXF-2, Name, EffectiveName
(defun mark:block-names (e / obj names nm)
  (setq names nil
        nm    (cdr (assoc 2 (entget e))))
  (if (mark:strp nm)
    (setq names (cons nm names)))
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list e)))
  (if (and (not (vl-catch-all-error-p obj)) obj)
    (progn
      (setq nm (mark:ax-get obj "Name"))
      (if (and (mark:strp nm) (not (member nm names)))
        (setq names (cons nm names)))
      (setq nm (mark:ax-get obj "EffectiveName"))
      (if (and (mark:strp nm) (not (member nm names)))
        (setq names (cons nm names)))))
  (reverse names))

;; Точное имя ИЛИ маска wcmatch в target (* ?)
(defun mark:name-match? (actual target / a t*)
  (and (mark:strp actual)
       (mark:strp target)
       (progn
         (setq a   (strcase (mark:trim actual))
               t*  (strcase (mark:trim target)))
         (or (= a t*)
             (wcmatch a t*)))))

(defun mark:blk-match? (e target / names nm ok)
  (setq ok    nil
        names (mark:block-names e))
  (foreach nm names
    (if (and (null ok) (mark:name-match? nm target))
      (setq ok t)))
  ok)

;;;--------------------- Атрибуты -------------------------------------

;; Поиск ATTRIB по DXF-цепочке INSERT(-2) > ATTRIB(-2) > ...
;; Возвращает ename атрибута или nil.
;; Принимает ename вставки.
;; Универсальный поиск: tag передаём явно
(defun mark:find-attr-in-ename (e tag / head ent data result)
  ;; 1) классическая цепочка INSERT -2
  (setq head   e
        result nil
        ent    (cdr (assoc -2 (entget e))))
  (while (and ent (null result))
    (if (eq ent head)
      (setq ent nil)
      (progn
        (setq data (entget ent))
        (cond
          ((null data)
           (setq ent nil))
          ((and (cdr (assoc 2 data))
                (mark:name= (cdr (assoc 2 data)) tag))
           (setq result ent))
          (t
           (setq ent (cdr (assoc -2 data))))))))
  ;; 2) fallback: entnext — ATTRIB/SEQEND сразу после INSERT (66=1)
  (if (null result)
    (progn
      (setq ent (entnext e))
      (while (and ent (null result))
        (setq data (entget ent))
        (cond
          ((null data)
           (setq ent nil))
          ((mark:name= (cdr (assoc 0 data)) "ATTRIB")
           (if (mark:name= (cdr (assoc 2 data)) tag)
             (setq result ent)
             (setq ent (entnext ent))))
          ((mark:name= (cdr (assoc 0 data)) "SEQEND")
           (setq ent nil))
          (t
           ;; не ATTRIB — дальше не ищем в этой ветке
           (setq ent nil))))))
  result)

;; Через ActiveX-коллекцию Attributes (без vla-get-*)
(defun mark:find-attr-obj (obj tag / result attrs i cnt a tagv)
  (setq result nil)
  (if obj
    (progn
      (setq attrs (mark:ax-get obj "Attributes"))
      (if attrs
        (progn
          (setq cnt (mark:ax-get attrs "Count"))
          (if (and (numberp cnt) (> cnt 0))
            (progn
              (setq i 0)
              (while (and (< i cnt) (null result))
                (setq a    (mark:ax-invoke attrs "Item" i)
                      tagv (if a (mark:ax-get a "TagString") nil))
                (if (and tagv (mark:name= tagv tag))
                  (setq result a))
                (setq i (1+ i)))))))))
  result)

;; Чтение значения атрибута по ename вставки: (T значение) | (NIL NIL)
(defun mark:find-attr (e tag / a data val obj)
  (setq a (mark:find-attr-in-ename e tag))
  (cond
    (a
     (progn
       (setq data (entget a)
             val  (cdr (assoc 1 data)))
       (list t (if (mark:strp val) val ""))))
    (t
     (progn
       (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list e)))
       (if (or (vl-catch-all-error-p obj) (null obj))
         (list nil nil)
         (progn
           (setq a (mark:find-attr-obj obj tag))
           (if a
             (progn
               (setq val (mark:ax-get a "TextString"))
               (list t (if (mark:strp val) val "")))
             (list nil nil))))))))

;;;--------------------- Dynamic Properties ---------------------------

;; (T значение) | (NIL NIL), если свойство недоступно
;; Развернуть VARIANT > значение
(defun mark:unwrap (v / w)
  (setq w (vl-catch-all-apply 'vlax-variant-value (list v)))
  (if (vl-catch-all-error-p w)
    v
    w))

;; Имя свойства dynamic block: PropertyName | Name | Description
(defun mark:dyn-item-name (item / nm)
  (setq nm (mark:ax-get item "PropertyName"))
  (if (not (mark:strp nm))
    (setq nm (mark:ax-get item "Name")))
  (if (not (mark:strp nm))
    (setq nm (mark:ax-get item "Description")))
  nm)

(defun mark:dyn-item-value (item / val)
  (setq val (mark:ax-get item "Value"))
  (if (null val)
    (setq val (mark:ax-get item "PropertyValue")))
  (mark:unwrap val))

;; Разбор одного элемента коллекции свойств > cons или nil
(defun mark:dyn-item-pair (item / nm val)
  (setq nm  (mark:dyn-item-name item)
        val (mark:dyn-item-value item))
  (if (mark:strp nm)
    (cons nm val)
    nil))

;; Пары (имя . значение) из GetDynamicBlockProperties
(defun mark:load-dyn-pairs (obj / r v lst item pair out)
  (setq out nil)
  (setq r (vl-catch-all-apply 'vlax-invoke-method
                              (list obj "GetDynamicBlockProperties")))
  (if (or (vl-catch-all-error-p r) (null r))
    nil
    (progn
      (setq v (mark:unwrap r))
      ;; safearray > list (без get-ubound)
      (setq lst (vl-catch-all-apply 'vlax-safearray->list (list v)))
      (cond
        ((and (not (vl-catch-all-error-p lst)) (listp lst))
         (progn
           (foreach item lst
             ;; item может быть variant
             (setq item (mark:unwrap item)
                   pair (mark:dyn-item-pair item))
             (if pair
               (setq out (cons pair out))))
           (reverse out)))
        (t
         (progn
           ;; не список — пробуем как одиночный объект
           (setq pair (mark:dyn-item-pair v))
           (if pair
             (list pair)
             (progn
               ;; последняя попытка: сам r как коллекция Count/Item
               (setq out nil
                     item (mark:ax-invoke r "Item" 0))
               (if item
                 (progn
                   (setq item (mark:unwrap item)
                         pair  (mark:dyn-item-pair item))
                   (if pair (setq out (list pair)))))
               out))))))))

;; Кэш: ename > alist
(defun mark:dyn-pairs (e / cache pair obj)
  (setq cache (assoc e *mark:dyn-cache*))
  (if cache
    (cdr cache)
    (progn
      (setq obj  (mark:vla e)
            pair (cons e (mark:load-dyn-pairs obj)))
      (setq *mark:dyn-cache* (cons pair *mark:dyn-cache*))
      (cdr pair))))

(defun mark:get-dyn (obj prop / r pr pairs v uv)
  (setq r (vl-catch-all-apply 'vlax-get-property (list obj prop)))
  (if (not (vl-catch-all-error-p r))
    (list t (mark:unwrap r))
    (progn
      (setq pr (vl-catch-all-apply 'vlax-vla-object->ename (list obj))
            pairs nil)
      (if (and (not (vl-catch-all-error-p pr)) pr)
        (setq pairs (mark:dyn-pairs pr)))
      (if (null pairs)
        (setq pairs (mark:load-dyn-pairs obj)))
      (setq v  (if (assoc prop pairs) (cdr (assoc prop pairs)) nil)
            uv (mark:unwrap v))
      (if uv
        (list t uv)
        (list nil (vl-catch-all-error-message r))))))

(defun mark:get-visibility (obj / cand r res)
  (setq res nil)
  (foreach cand *mark:prop-vis-candidates*
    (if (null res)
      (progn
        (setq r (mark:get-dyn obj cand))
        (if (car r)
          (setq res (list t cand (cadr r)))))))
  res)

;;;--------------------- Выбор объектов -------------------------------

(defun mark:select (/ ss i e fills glazings others names nm lst)
  (if (and *mark:reuse-sel* *mark:fills*)
    (progn
      (mark:out "[INFO] Выбор уже выполнен (МАРКА) — без повторного выделения.")
      t)
    (progn
  (mark:out "")
  (prompt
    "\nВыберите блоки «Заполнение в витраж» (стойки не нужны): ")
  (setq ss (vl-catch-all-apply 'ssget nil))
  (cond
    ((vl-catch-all-error-p ss)
     (mark:out "[ERROR] Ошибка при выделении.")
     (mark:note-error)
     nil)
    ((null ss)
     (mark:out "[INFO] Выбор отменён. Изменений не выполнено.")
     nil)
    (t
     (setq fills     nil
           glazings  nil
           others    0
           names     nil
           i         (sslength ss))
     (repeat i
       (setq i (1- i)
             e (ssname ss i))
       (setq lst (mark:block-names e))
       (foreach nm lst
         (if (not (member nm names))
           (setq names (cons nm names))))
       (cond
         ((mark:blk-match? e *mark:block-fill*)
          (setq fills (cons e fills)))
         ((mark:blk-match? e *mark:block-glazing*)
          (setq glazings (cons e glazings)))
         (t (setq others (1+ others)))))
     (setq *mark:sel-total*   (sslength ss)
           *mark:fills*        (reverse fills)
           *mark:glazings*     (reverse glazings)
           *mark:others*       others
           *mark:found-names*  (reverse names))
     (mark:out "")
     (mark:out (strcat "[INFO] Выбрано объектов: " (itoa *mark:sel-total*)))
     (mark:out (strcat "[INFO] Заполнений: " (itoa (length *mark:fills*))))
     (mark:out
       (strcat "[INFO] \"Атрибуты витража\": "
               (itoa (length *mark:glazings*))))
     (if (> others 0)
       (mark:out
         (strcat "[INFO] Прочих объектов (игнорируются): " (itoa others))))
     (mark:out "[INFO] Имена вставок в выделении:")
     (if *mark:found-names*
       (foreach nm *mark:found-names*
         (mark:out (strcat "         - " nm)))
       (mark:out "         (имена не прочитаны)"))
     (if (null *mark:fills*)
       (progn
         (mark:out "[INFO] Имя/маска заполнения (*mark:block-fill*):")
         (mark:out (strcat "         " *mark:block-fill*))))
     t)))))

;;;--------------------- Сбор данных (только чтение) ------------------

;; Все теги ATTRIB вставки (DXF-цепочка -2)
(defun mark:attr-tags (e / head ent data tags tag)
  (setq head e
        tags nil
        ent  (cdr (assoc -2 (entget e))))
  (while (and ent (not (eq ent head)))
    (setq data (entget ent))
    (if (null data)
      (setq ent nil)
      (progn
        (setq tag (cdr (assoc 2 data)))
        (if (mark:strp tag)
          (setq tags (cons tag tags)))
        (setq ent (cdr (assoc -2 data))))))
  ;; fallback entnext
  (if (null tags)
    (progn
      (setq ent (entnext e))
      (while (and ent)
        (setq data (entget ent))
        (cond
          ((null data)
           (setq ent nil))
          ((mark:name= (cdr (assoc 0 data)) "ATTRIB")
           (setq tag (cdr (assoc 2 data)))
           (if (mark:strp tag)
             (setq tags (cons tag tags)))
           (setq ent (entnext ent)))
          ((mark:name= (cdr (assoc 0 data)) "SEQEND")
           (setq ent nil))
          (t
           (setq ent nil))))))
  (reverse tags))

;; Теги через ActiveX-коллекцию Attributes
(defun mark:attr-tags-ax (obj / attrs cnt i a tag tags)
  (setq tags nil
        attrs (mark:ax-get obj "Attributes"))
  (if attrs
    (progn
      (setq cnt (mark:ax-get attrs "Count"))
      (if (and (numberp cnt) (> cnt 0))
        (progn
          (setq i 0)
          (while (< i cnt)
            (setq a   (mark:ax-invoke attrs "Item" i)
                  tag (if a (mark:ax-get a "TagString") nil))
            (if (mark:strp tag)
              (setq tags (cons tag tags)))
            (setq i (1+ i)))))))
  (reverse tags))

(defun mark:collect-data (/ recs idx e obj wh hh vv mr)
  (setq recs     nil
        idx      0
        *mark:cnt-stem* 0
        *mark:cnt-sand* 0)
  (foreach e *mark:fills*
    (setq idx (1+ idx))
    (setq obj (mark:vla e))
    (setq wh (mark:get-dyn obj *mark:prop-width*))
    (setq hh (mark:get-dyn obj *mark:prop-height*))
    (setq vv (mark:get-visibility obj))
    (setq mr (mark:find-attr e *mark:attr-mark*))
    (if vv
      (progn
        (if (= (mark:vis-rank (caddr vv)) 1)
          (setq *mark:cnt-stem* (1+ *mark:cnt-stem*)))
        (if (= (mark:vis-rank (caddr vv)) 2)
          (setq *mark:cnt-sand* (1+ *mark:cnt-sand*)))))
    (setq recs
      (cons
        (list
          (cons 'idx        idx)
          (cons 'ename      e)
          (cons 'width-ok   (car wh))
          (cons 'width-err  (if (car wh) nil (cadr wh)))
          (cons 'width-raw  (cadr wh))
          (cons 'width      (mark:numval (cadr wh)))
          (cons 'height-ok  (car hh))
          (cons 'height-err (if (car hh) nil (cadr hh)))
          (cons 'height-raw (cadr hh))
          (cons 'height     (mark:numval (cadr hh)))
          (cons 'vis-ok     (if vv t nil))
          (cons 'vis-prop   (if vv (cadr vv) nil))
          (cons 'vis        (if vv (caddr vv) nil))
          (cons 'mark-ok    (car mr))
          (cons 'mark-old   (cadr mr))
          (cons 'letter     nil)
          (cons 'hnum       nil)
          (cons 'mark-new   nil))
        recs)))
  (setq *mark:records* (reverse recs))
  *mark:records*)

;;;--------------------- TEST 00 — прогон скобок исходника ------------

(setq *mark:source-candidates*
  '("D:/MarkZ/MARKZ.lsp"
    "D:\\MarkZ\\MARKZ.lsp"
    "D:/MARKZ.lsp"
    "D:\\MARKZ.lsp"
    "MARKZ.lsp"))

(setq *mark:source-file* nil)

(defun mark:file-open? (path / f)
  (and (mark:strp path)
       (/= path "")
       (setq f (vl-catch-all-apply 'open (list path "r")))
       (not (vl-catch-all-error-p f))
       f
       (progn (close f) t)))

;; Не привязан к одному диску: findfile (пути поддержки) и явные кандидаты.
(defun mark:find-source (/ cand found ff)
  (setq found nil
        ff    (vl-catch-all-apply 'findfile (list "MARKZ.lsp")))
  (if (and (not (vl-catch-all-error-p ff)) (mark:file-open? ff))
    (setq found ff))
  (if (null found)
    (foreach cand *mark:source-candidates*
      (if (null found)
        (progn
          (setq ff (vl-catch-all-apply 'findfile (list cand)))
          (cond
            ((and (not (vl-catch-all-error-p ff)) (mark:file-open? ff))
             (setq found ff))
            ((mark:file-open? cand)
             (setq found cand)))))))
  (setq *mark:source-file* found)
  found)

;; Сканер: баланс () вне строк и комментариев.
;; Возвращает (T "детали OK") или (NIL "детали ошибка")
(defun mark:scan-parens (path / f ch depth in-str in-com line err n-open n-close
                            n-str-start escaped cr)
  (setq f (vl-catch-all-apply 'open (list path "r")))
  (if (or (vl-catch-all-error-p f) (null f))
    (list nil (strcat "файл не открыт: " path))
    (progn
      (setq depth   0
            in-str  nil
            in-com  nil
            line    1
            err     nil
            n-open  0
            n-close 0
            n-str-start 0)
      (while (and (null err) (setq ch (read-char f)))
        (cond
          ;; перевод строки
          ((= ch 10)
           (setq in-com nil
                 line   (1+ line)))
          ;; внутри комментария — до конца строки
          (in-com nil)
          ;; внутри строки
          (in-str
           (cond
             ((= ch 92)  ; backslash — экранирующий символ
              (if (null (read-char f))
                (setq err (strcat "незавершённый escape, строка " (itoa line)))))
             ((= ch 34)  ; закрывающая кавычка
              (setq in-str nil))
             (t nil)))
          ;; обычный код
          ((= ch 59) (setq in-com t))          ; ; — комментарий
          ((= ch 34)                           ; " — открытие строки
           (setq in-str t
                 n-str-start (1+ n-str-start)))
          ((= ch 40)                           ; (
           (setq depth   (1+ depth)
                 n-open  (1+ n-open)))
          ((= ch 41)                           ; )
           (setq depth    (1- depth)
                 n-close  (1+ n-close))
           (if (< depth 0)
             (setq err (strcat "лишняя закрывающая ) — строка " (itoa line)))))
          (t nil)))
      (close f)
      (cond
        (err
         (list nil err))
        (in-str
         (list nil (strcat "незакрытая строка \"..., строка " (itoa line))))
        ((/= depth 0)
         (list nil
               (strcat "дисбаланс глубины " (itoa depth)
                       " (открыто " (itoa n-open)
                       ", закрыто " (itoa n-close) ")")))
        (t
         (list t
               (strcat "OK, открытых (: " (itoa n-open)
                       ", закрытых ): " (itoa n-close)
                       ", строк: " (itoa line))))))))

(defun mark:test-parens (/ path res)
  (mark:out "")
  (setq path (mark:find-source))
  (cond
    ((null path)
     (progn
       (mark:out "[TEST 00] Прогон скобок — WARNING")
       (mark:out "Файл исходника для прогона скобок не найден.")
       (mark:out "[INFO] Искали findfile MARKZ.lsp, D:/MarkZ/MARKZ.lsp и D:/MARKZ.lsp.")
       (mark:note-warning)))
    (t
     (progn
       (setq res (mark:scan-parens path))
       (cond
         ((car res)
          (progn
            (mark:out "[TEST 00] Прогон скобок — OK")
            (mark:out (strcat "Файл: " path))
            (mark:out (cadr res))))
         (t
          (progn
            (mark:out "[TEST 00] Прогон скобок — ERROR")
            (mark:out (strcat "Файл: " path))
            (mark:out (strcat "Синтаксис исходника нарушен: " (cadr res)))
            (mark:out "[ERROR] Немедленно исправьте MARKZ.lsp — запись запрещена.")
            (mark:note-error))))))))

;;;--------------------- TEST 01 — блоки заполнений -------------------

(defun mark:test-fillings ()
  (mark:out "")
  (if (> (length *mark:fills*) 0)
    (progn
      (mark:out "[TEST 01] Блоки \"Заполнение в витраж\" — OK")
      (mark:out (strcat "Найдено: " (itoa (length *mark:fills*)))))
    (progn
      (mark:out "[TEST 01] Блоки \"Заполнение в витраж\" — ERROR")
      (mark:out "Блоки заполнений не обнаружены.")
      (mark:out "[INFO] МАРКАРОВКА пишет марку только в «Заполнение в витраж».")
      (mark:out "[INFO] Стойки и линии сетки эта команда не маркирует.")
      (mark:out "[INFO] Сетка без заполнений — команда МАРКА, источник Сетка.")
      (mark:out "[INFO] Имена, найденные в выделении:")
      (if *mark:found-names*
        (foreach nm *mark:found-names*
          (mark:out (strcat "         - " nm))))
      (mark:out "[INFO] Ожидалось (*mark:block-fill*):")
      (mark:out (strcat "         " *mark:block-fill*))
      (mark:note-error))))

;;;--------------------- TEST 02 / 03 — Атрибуты витража --------------

(defun mark:test-glazing-attr (/ g obj res val)
  (mark:out "")
  (cond
    ;; не найден — не ошибка, работа без префикса
    ((null *mark:glazings*)
     (mark:out "[TEST 02] \"Атрибуты витража\" — NOT FOUND")
     (mark:out "Работа без префикса \"Витраж\".")
     (mark:out "[INFO] Блок \"Атрибуты витража\" не найден.")
     (mark:out "[INFO] Маркировка будет выполнена без префикса \"Витраж\".")
     (setq *mark:prefix* ""
           *mark:prefix-found* nil))
    (t
     (mark:out "[TEST 02] \"Атрибуты витража\" — FOUND")
     (mark:out (strcat "Количество: " (itoa (length *mark:glazings*))))
     (if (> (length *mark:glazings*) 1)
       (progn
         (mark:out
           "[WARN] Найдено несколько блоков \"Атрибуты витража\". Используется первый.")
         (mark:note-warning)))
     (setq g   (car *mark:glazings*)
           obj (mark:vla g)
           res (mark:find-attr g *mark:attr-vitrage*))
     (mark:out "")
     (cond
       ;; атрибута нет — критическая ошибка
       ((null (car res))
        (mark:out "[TEST 03] Атрибут \"Витраж\" — ERROR")
        (mark:out "В блоке \"Атрибуты витража\" атрибут не найден.")
        (mark:note-error)
        (setq *mark:prefix* ""
              *mark:prefix-found* nil))
       ;; пустое значение — WARNING, по умолчанию блокирует запись
       ((= (mark:trim (cadr res)) "")
        (mark:out "[TEST 03] Атрибут \"Витраж\" — WARNING")
        (mark:out "Атрибут найден, но значение пустое.")
        (mark:out
          "[ERROR] По умолчанию это критическая ошибка. Маркировка будет остановлена.")
        (mark:note-warning)
        (mark:note-error)
        (setq *mark:prefix* ""
              *mark:prefix-found* nil))
       ;; успех
       (t
        (setq val (mark:trim (cadr res)))
        (mark:out "[TEST 03] Атрибут \"Витраж\" — OK")
        (mark:out (strcat "Значение: " val))
        (setq *mark:prefix* val
              *mark:prefix-found* t))))))

;;;--------------------- TEST 04/05/06 — размеры ----------------------

(defun mark:test-dynamic-properties (/ bad4 bad5 bad6 r idx lines)
  (setq bad4 nil
        bad5 nil
        bad6 nil)
  (foreach r *mark:records*
    (setq idx (mark:rec-get r 'idx))
    (if (null (mark:rec-get r 'width-ok))
      (setq bad4 (cons idx bad4)))
    (if (null (mark:rec-get r 'height-ok))
      (setq bad5 (cons idx bad5)))
    ;; TEST 06 — существуют, числовые, > 0
    (cond
      ((null (mark:rec-get r 'width-ok))
       (setq bad6 (cons (list idx "Ширина в свету" "NIL") bad6)))
      ((null (mark:rec-get r 'width))
       (setq bad6
         (cons (list idx "Ширина в свету"
                     (mark:fmt-raw (mark:rec-get r 'width-raw)))
               bad6)))
      ((not (> (mark:rec-get r 'width) 0.0))
       (setq bad6
         (cons (list idx "Ширина в свету"
                     (mark:fmt-raw (mark:rec-get r 'width)))
               bad6))))
    (cond
      ((null (mark:rec-get r 'height-ok))
       (setq bad6 (cons (list idx "Высота в свету" "NIL") bad6)))
      ((null (mark:rec-get r 'height))
       (setq bad6
         (cons (list idx "Высота в свету"
                     (mark:fmt-raw (mark:rec-get r 'height-raw)))
               bad6)))
      ((not (> (mark:rec-get r 'height) 0.0))
       (setq bad6
         (cons (list idx "Высота в свету"
                     (mark:fmt-raw (mark:rec-get r 'height)))
               bad6)))))
  (setq bad4 (reverse bad4)
        bad5 (reverse bad5)
        bad6 (reverse bad6))

  ;; TEST 04
  (mark:out "")
  (cond
    ((null bad4)
     (mark:out "[TEST 04] \"Ширина в свету\" — OK"))
    (t
     (mark:out "[TEST 04] \"Ширина в свету\" — ERROR")
     (setq lines nil)
     (foreach r *mark:records*
       (if (null (mark:rec-get r 'width-ok))
         (setq lines
           (cons (strcat "Блок №" (itoa (mark:rec-get r 'idx))
                         ": "
                         (if (mark:rec-get r 'width-err)
                           (mark:rec-get r 'width-err)
                           "свойство не найдено"))
                 lines))))
     (mark:print-limited (reverse lines))
     (mark:out "[INFO] Имя свойства должно точно совпадать: \"Ширина в свету\".")
     (mark:note-error)))

  ;; TEST 05
  (mark:out "")
  (cond
    ((null bad5)
     (mark:out "[TEST 05] \"Высота в свету\" — OK"))
    (t
     (mark:out "[TEST 05] \"Высота в свету\" — ERROR")
     (setq lines nil)
     (foreach r *mark:records*
       (if (null (mark:rec-get r 'height-ok))
         (setq lines
           (cons (strcat "Блок №" (itoa (mark:rec-get r 'idx))
                         ": "
                         (if (mark:rec-get r 'height-err)
                           (mark:rec-get r 'height-err)
                           "свойство не найдено"))
                 lines))))
     (mark:print-limited (reverse lines))
     (mark:out "[INFO] Имя свойства должно точно совпадать: \"Высота в свету\".")
     (mark:note-error)))

  ;; TEST 06
  (mark:out "")
  (cond
    ((null bad6)
     (progn
       (mark:out "[TEST 06] Значения размеров — OK")
       (mark:out (strcat "Обработано: " (itoa (length *mark:records*))))))
    (t
     (progn
       (mark:out "[TEST 06] Значения размеров — ERROR")
       (setq lines nil)
       (foreach r bad6
         (setq lines
           (cons (strcat "Блок №" (itoa (car r)) ": " (cadr r) " = " (caddr r))
                 lines)))
       (mark:print-limited (reverse lines))
       (mark:note-error)))))

;;;--------------------- TEST 07 — Visibility --------------------------

(defun mark:test-visibility (/ bad r lines)
  (setq bad nil)
  (foreach r *mark:records*
    (if (null (mark:rec-get r 'vis-ok))
      (setq bad (cons (mark:rec-get r 'idx) bad))))
  (setq bad (reverse bad))
  (mark:out "")
  (cond
    ((null bad)
     (mark:out "[TEST 07] Visibility State — OK"))
    (t
     (progn
       (mark:out "[TEST 07] Visibility State — ERROR")
       (setq lines nil)
       (foreach idx bad
         (setq lines
           (cons (strcat "Блок №" (itoa idx)
                         ": невозможно определить состояние Visibility.")
                 lines)))
       (mark:print-limited (reverse lines))
       (mark:note-error)
       (mark:out "[INFO] Проверьте имя Dynamic Property Visibility")
       (mark:out "       в списке *mark:prop-vis-candidates*.")))))

;;;--------------------- TEST 08 — атрибут "Марка" ---------------------

(defun mark:test-mark-attribute (/ bad r lines)
  (setq bad nil)
  (foreach r *mark:records*
    (if (null (mark:rec-get r 'mark-ok))
      (setq bad (cons (mark:rec-get r 'idx) bad))))
  (setq bad (reverse bad))
  (mark:out "")
  (cond
    ((null bad)
     (progn
       (mark:out "[TEST 08] Атрибут \"Марка\" — OK")
       (mark:out
         (strcat "Найден у всех " (itoa (length *mark:records*)) " блоков."))))
    (t
     (progn
       (mark:out "[TEST 08] Атрибут \"Марка\" — ERROR")
       (setq lines nil)
       (foreach idx bad
         (setq lines
           (cons (strcat "Блок №" (itoa idx) ": атрибут \"Марка\" отсутствует.")
                 lines)))
       (mark:print-limited (reverse lines))
       (mark:note-error)))))

;;;--------------------- TEST 09/10/11 — данные и нумерация ------------

(defun mark:test-data (/ wlist hlist)
  (setq wlist
        (mark:sort-unique
          (mark:positives
            (mapcar (function (lambda (r) (mark:rec-get r 'width)))
                    *mark:records*))))
  (setq hlist
        (mark:sort-unique
          (mark:positives
            (mapcar (function (lambda (r) (mark:rec-get r 'height)))
                    *mark:records*))))
  (setq *mark:widths*  wlist
        *mark:heights* hlist)

  ;; TEST 09
  (mark:out "")
  (cond
    ((and wlist hlist)
     (progn
       (mark:out "[TEST 09] Уникальные размеры — OK")
       (mark:out "")
       (mark:out (strcat "Уникальных ширин: " (itoa (length wlist))))
       (mark:out (strcat "Уникальных высот: " (itoa (length hlist))))
       (mark:out "")
       (mark:out
         (strcat "Ширина: " (mark:fmt-raw (car wlist))
                 " ... " (mark:fmt-raw (car (reverse wlist)))))
       (mark:out
         (strcat "Высота: " (mark:fmt-raw (car hlist))
                 " ... " (mark:fmt-raw (car (reverse hlist)))))))
    (t
     (progn
       (mark:out "[TEST 09] Уникальные размеры — ERROR")
       (mark:out "Нет корректных значений ширины/высоты.")
       (mark:note-error))))

  ;; TEST 10 — буквы по высотам. После списка продолжаем АА, АБ, … БА, ББ.
  (mark:out "")
  (mark:out "[TEST 10] Буквенная маркировка — OK")
  (mark:out (strcat "Уникальных высот: " (itoa (length hlist))))
  (mark:out
    (strcat "Букв в списке: " (itoa (length *mark:letters*))
            ". Дальше АА, АБ, … БА, ББ."))

  ;; TEST 11 — номера по ширинам
  (mark:out "")
  (cond
    ((> (length wlist) 0)
     (progn
       (mark:out "[TEST 11] Нумерация ширин — OK")
       (mark:out (strcat "Уникальных ширин: " (itoa (length wlist))))))
    (t
     (progn
       (mark:out "[TEST 11] Нумерация ширин — ERROR")
       (mark:out "Уникальных ширин: 0")
       (mark:note-error)))))

;;;--------------------- TEST 12 — специальные Visibility --------------

(defun mark:test-specials ()
  (mark:out "")
  (mark:out "[TEST 12] Специальные состояния Visibility")
  (mark:out "")
  (if (> *mark:cnt-stem* 0)
    (mark:out (strcat "Стемалит — найдено: " (itoa *mark:cnt-stem*)))
    (mark:out "Стемалит — не найден"))
  (if (> *mark:cnt-sand* 0)
    (mark:out (strcat "Сэндвич   — найдено: " (itoa *mark:cnt-sand*)))
    (mark:out "Сэндвич   — не найден")))

;;;--------------------- Сортировка и сборка марок ---------------------

(defun mark:sort-widths ()
  *mark:widths*)

(defun mark:sort-heights ()
  *mark:heights*)

(defun mark:get-special-suffix (vis / result sp)
  (setq result "")
  (foreach sp *mark:specials*
    (if (and (mark:strp vis)
             (or (mark:name= vis (car sp))
                 (wcmatch (strcase (mark:trim vis))
                          (strcase (strcat (car sp) "*")))))
      (setq result (cdr sp))))
  result)

(defun mark:vis-rank (vis)
  (cond
    ((and (mark:strp vis)
          (or (mark:name= vis "Стемалит")
              (wcmatch (strcase (mark:trim vis)) "СТЕМАЛИТ*")))
     1)
    ((and (mark:strp vis)
          (or (mark:name= vis "Сэндвич")
              (wcmatch (strcase (mark:trim vis)) "СЭНДВИЧ*")))
     2)
    (t 0)))

(defun mark:compose (prefix letter hnum suffix)
  (strcat
    (if (and prefix (/= prefix "") letter)
      (strcat prefix " ")
      "")
    (if letter letter "")
    (if hnum (itoa hnum) "")
    (if suffix suffix "")))

(defun mark:build-marks (/ out r w h iw ih letter hnum suffix m)
  (setq out nil)
  (foreach r *mark:records*
    (setq w      (mark:rec-get r 'width)
          h      (mark:rec-get r 'height)
          iw     (if w (mark:index-of *mark:widths* w) nil)
          ih     (if h (mark:index-of *mark:heights* h) nil)
          ;; буква — вертикальная рядовка (высота), номер — горизонтальная (ширина)
          letter (if ih (mark:letter-at ih) nil)
          hnum   (if iw (1+ iw) nil)
          suffix (mark:get-special-suffix (mark:rec-get r 'vis))
          m      (if (and letter hnum)
                   (mark:compose *mark:prefix* letter hnum suffix)
                   nil))
    (setq r (mark:rec-put r 'letter   letter))
    (setq r (mark:rec-put r 'hnum     hnum))
    (setq r (mark:rec-put r 'mark-new m))
    (setq out (cons r out)))
  (setq *mark:records* (reverse out))
  *mark:records*)

;;;--------------------- TEST 13 — марки в памяти ----------------------

(defun mark:num-safe (x)
  (cond
    ((numberp x) (float x))
    (t
     (setq x (mark:numval x))
     (if (numberp x)
       (float x)
       0.0))))

(defun mark:rec-key (r / w h vr ix)
  (setq w  (mark:num-safe (mark:rec-get r 'width))
        h  (mark:num-safe (mark:rec-get r 'height))
        vr (mark:vis-rank (mark:rec-get r 'vis))
        ix (mark:rec-get r 'idx))
  (list w
        h
        (if (numberp vr) (float vr) 0.0)
        (if (numberp ix) (float ix) 0.0)))

(defun mark:key< (a b / i av bv)
  ;; поэлементное сравнение только чисел (никогда T/строки)
  (setq i 0)
  (while (< i 4)
    (setq av (nth i a)
          bv (nth i b))
    (if (not (and (numberp av) (numberp bv)))
      (progn
        (setq av (mark:num-safe av)
              bv (mark:num-safe bv))))
    (cond
      ((< av bv)
       (setq i 10)
       (setq mark:key<:result t))
      ((> av bv)
       (setq i 10)
       (setq mark:key<:result nil))
      (t
       (setq i (1+ i)))))
  (if (> i 4)
    mark:key<:result
    nil))

(defun mark:sort-records (recs / res r inserted lst)
  ;; сортировка для вывода: ширина ^, высота ^, Visibility (спец. — в конце)
  (setq res nil)
  (foreach r recs
    (setq inserted nil
          lst      nil)
    (foreach x res
      (if (and (null inserted)
               (mark:key< (mark:rec-key r) (mark:rec-key x)))
        (progn
          (setq lst      (cons r lst)
                inserted t)))
      (setq lst (cons x lst)))
    (if (null inserted)
      (setq lst (cons r lst)))
    (setq res (reverse lst)))
  res)

;;;--- DIAG: одинаковый размер — разный базис (стм/снд vs обычное) ----
;; Группирует записи по ключу (W,H) с допуском 0.5 мм.
;; В группе сравнивает (letter . hnum) — базис без суффикса.
(defun mark:diag-base (/ groups g gg ng key r w h base bases
                             lines shown total conflicts
                             letter hnum suffix
                             kinds kind)
  (mark:out "")
  (mark:out "[DIAG] Одинаковый размер > один базис (буква+номер)")
  (setq groups nil)
  (foreach r *mark:records*
    (setq w (mark:rec-get r 'width)
          h (mark:rec-get r 'height))
    (if (and (numberp w) (numberp h))
      (progn
        (setq key (list (mark:dim-key w) (mark:dim-key h))
              g   nil)
        (foreach gg groups
          (if (equal (car gg) key)
            (setq g gg)))
        (if g
          (progn
            (setq ng nil)
            (foreach gg groups
              (setq ng
                (cons (if (equal (car gg) key)
                        (append (list (car gg)) (list r) (cdr gg))
                        gg)
                      ng)))
            (setq groups (reverse ng)))
          (setq groups (cons (list key r) groups))))))
  (setq conflicts 0
        lines     nil)
  (foreach g groups
    (setq bases nil
          kinds  nil)
    (foreach r (cdr g)
      (setq letter (mark:rec-get r 'letter)
            hnum   (mark:rec-get r 'hnum)
            suffix (mark:get-special-suffix (mark:rec-get r 'vis))
            base   (list (if letter letter "?")
                         (if hnum (itoa hnum) "?")))
      (if (not (member base bases))
        (setq bases (cons base bases)))
      (setq kind (if (= suffix "") "обычное" suffix))
      (if (not (member kind kinds))
        (setq kinds (cons kind kinds))))
    (if (> (length bases) 1)
      (progn
        (setq conflicts (1+ conflicts))
        (setq lines
          (cons (strcat
                  "  W=" (rtos (car (car g)) 2 0)
                  " H=" (rtos (cadr (car g)) 2 0)
                  " half-mm > базисы: "
                  (mark:diag-bases-str bases)
                  " | типы: " (mark:diag-join kinds))
                lines)))))
  (if (null lines)
    (mark:out "[DIAG] OK — расхождений базиса нет.")
    (progn
      (mark:out (strcat "[DIAG] КОНФЛИКТОВ: " (itoa conflicts)))
      (setq lines (reverse lines)
            shown 0
            total (length lines))
      (foreach ln lines
        (if (< shown 12)
          (progn
            (setq shown (1+ shown))
            (mark:out ln))))
      (if (> total shown)
        (mark:out (strcat "  ... ещё " (itoa (- total shown)) " строк"))
        nil)
      (mark:out "[DIAG] Причина: разные фактические W/H у блоков")
      (mark:out "        (сверьте «Ширина/Высота в свету» у конфликтов).")))
  conflicts)

(defun mark:diag-bases-str (bases / out b)
  (setq out "")
  (foreach b bases
    (setq out
      (if (= out "")
        (strcat (car b) (cadr b))
        (strcat out ", " (car b) (cadr b)))))
  out)

(defun mark:diag-join (ks / out k)
  (setq out "")
  (foreach k ks
    (setq out
      (if (= out "") k (strcat out "+" k))))
  out)

(defun mark:validate-marks (/ bad sorted r shown total lines)
  (mark:out "")
  (setq bad nil)
  (foreach r *mark:records*
    (if (null (mark:rec-get r 'mark-new))
      (setq bad (cons (mark:rec-get r 'idx) bad))))
  (setq bad (reverse bad))
  (cond
    (bad
     (progn
       (mark:out "[TEST 13] Проверка формирования марок — ERROR")
       (setq lines nil)
       (foreach idx bad
         (setq lines
           (cons (strcat "Блок №" (itoa idx) ": марка не сформирована.") lines)))
       (mark:print-limited (reverse lines))
       (mark:note-error)))
    (t
     (progn
       (mark:out "[TEST 13] Проверка формирования марок — OK")
       (mark:out "")
       (if (not (numberp *mark:test13-show*))
         (setq *mark:test13-show* 10))
       (setq sorted (vl-catch-all-apply 'mark:sort-records
                                        (list *mark:records*)))
       (if (or (vl-catch-all-error-p sorted) (null sorted))
         (setq sorted *mark:records*))
       (setq total (length sorted)
             shown 0)
       (foreach r sorted
         (if (< shown *mark:test13-show*)
           (progn
             (setq shown (1+ shown))
             (mark:out (strcat "Блок " (itoa (mark:rec-get r 'idx)) ":"))
             (mark:out
               (strcat (mark:fmt-raw (mark:rec-get r 'width))
                       " x "
                       (mark:fmt-raw (mark:rec-get r 'height))
                       " / "
                       (if (mark:rec-get r 'vis)
                         (mark:rec-get r 'vis)
                         "—")))
             (mark:out (strcat "> " (mark:rec-get r 'mark-new)))
             (mark:out ""))))
       (if (> total *mark:test13-show*)
         (mark:out
           (strcat "... (показаны первые " (itoa *mark:test13-show*)
                   " из " (itoa total) ")")))
       (mark:out (strcat "Всего блоков: " (itoa total)))
       (mark:out (strcat "Сформировано марок: " (itoa total)))))))

;;;--------------------- TEST 14 — PRE-CHECK ---------------------------

(defun mark:pre-check ()
  (mark:out "")
  (cond
    ((= *mark:errors* 0)
     (progn
       (setq *mark:precheck-ok* t)
       (mark:out "[TEST 14] PRE-CHECK — OK")
       (mark:out "Критических ошибок: 0")
       (mark:out (strcat "Предупреждений: " (itoa *mark:warnings*)))
       (mark:out "Готово к записи атрибутов.")))
    (t
     (progn
       (setq *mark:precheck-ok* nil)
       (mark:out "[TEST 14] PRE-CHECK — FAILED")
       (mark:out (strcat "Критических ошибок: " (itoa *mark:errors*)))
       (mark:out (strcat "Предупреждений: " (itoa *mark:warnings*)))
       (mark:out "Маркировка отменена.")
       (mark:out "Изменений в чертеже не выполнено.")))))



;;;--------------------- Глубокий разбор структуры --------------------

(defun mark:tags-str (tags)
  (if tags (vl-princ-to-string tags) "(нет)"))

;; Обычные свойства — проверка, что COM работает
(defun mark:debug-std-props (obj)
  (mark:out (strcat "  Layer: "
                    (vl-princ-to-string (mark:ax-get obj "Layer"))))
  (mark:out (strcat "  Color: "
                    (vl-princ-to-string (mark:ax-get obj "Color")))))

;; Проба списка имён Dynamic Properties
(defun mark:debug-try-names (obj names / cand r okn bad)
  (setq okn nil)
  (foreach cand names
    (setq r (mark:get-dyn obj cand))
    (if (car r)
      (setq okn (cons (strcat cand " = " (vl-princ-to-string (cadr r)))
                      okn))))
  (if okn
    (progn
      (mark:out "  Успешные имена Dynamic Properties:")
      (foreach x (reverse okn)
        (mark:out (strcat "    OK  " x))))
    (mark:out "  Успешных имён среди проб — нет")))

;; GetDynamicBlockProperties (метод есть не во всех версиях)
(defun mark:debug-enum-dyn (obj / r v lst item pair n)
  (setq r (vl-catch-all-apply 'vlax-invoke-method
                              (list obj "GetDynamicBlockProperties")))
  (cond
    ((vl-catch-all-error-p r)
     (mark:out (strcat "  GetDynamicBlockProperties: FAIL — "
                       (vl-catch-all-error-message r))))
    ((null r)
     (mark:out "  GetDynamicBlockProperties: nil"))
    (t
     (progn
       (mark:out "  GetDynamicBlockProperties: получен")
       (mark:out (strcat "    type = " (vl-princ-to-string (type r))))
       (setq v (mark:unwrap r))
       (mark:out (strcat "    unwrap type = " (vl-princ-to-string (type v))))
       ;; safearray > list (только эта функция)
       (setq lst (vl-catch-all-apply 'vlax-safearray->list (list v)))
       (cond
         ((vl-catch-all-error-p lst)
          (mark:out (strcat "    safearray->list FAIL: "
                            (vl-catch-all-error-message lst))))
         ((null lst)
          (mark:out "    safearray->list: пустой список"))
         ((not (listp lst))
          (mark:out (strcat "    safearray->list: не список, type="
                            (vl-princ-to-string (type lst)))))
         (t
          (progn
            (mark:out (strcat "    элементов: "
                              (itoa (length lst))))
            (setq n 0)
            (foreach item lst
              (setq n (1+ n))
              (if (<= n 30)
                (progn
                  (setq item (mark:unwrap item)
                        pair (mark:dyn-item-pair item))
                  (if pair
                    (mark:out
                      (strcat "      * " (car pair)
                              " = " (vl-princ-to-string (cdr pair))))
                    (mark:out
                      (strcat "      ? item type="
                              (vl-princ-to-string (type item))
                              " PropertyName="
                              (vl-princ-to-string
                                (mark:ax-get item "PropertyName"))
                              " Name="
                              (vl-princ-to-string
                                (mark:ax-get item "Name"))))))))))
       ;; итог через общий загрузчик
       (setq pair (mark:load-dyn-pairs obj))
       (if pair
         (progn
           (mark:out "    --- mark:load-dyn-pairs ---")
           (foreach item pair
             (mark:out (strcat "      = " (car item)
                               " : " (vl-princ-to-string (cdr item))))))
         (mark:out "    load-dyn-pairs: nil")))))))


(defun mark:debug-entget (e / pair k v)
  (mark:out "  --- entget (ключевые группы) ---")
  (foreach pair (entget e)
    (setq k (car pair)
          v (cdr pair))
    (if (member k '(-2 -1 0 2 8 60 62 66 280 284 410 1 3 7 1000 1001 1070 1071 1040))
      (mark:out (strcat "    " (vl-princ-to-string k) " = "
                        (vl-princ-to-string v))))))

;; XDATA 1000–1071
(defun mark:debug-xdata (e / pair items)
  (mark:out "  --- XDATA 1000-1071 ---")
  (setq items nil)
  (foreach pair (entget e)
    (if (and (numberp (car pair))
             (>= (car pair) 1000)
             (<= (car pair) 1071))
      (setq items (cons pair items))))
  (if items
    (foreach pair (reverse items)
      (mark:out (strcat "    " (vl-princ-to-string pair))))
    (mark:out "    (нет)")))

;; Перечисление ATTDEF/ATTRIB/TEXT в определении блока (только имена типов)
(defun mark:debug-block-def (bname / doc blocks rec cnt i item tn en data)
  (cond
    ((null (mark:strp bname))
     (mark:out "  block-def: имя не строка"))
    (t
     (progn
       (mark:out (strcat "  block-def: " bname))
       (setq doc    (mark:ax-get (vlax-get-acad-object) "ActiveDocument")
             blocks (mark:ax-get doc "Blocks")
             rec    (mark:ax-invoke blocks "Item" bname))
       (if (null rec)
         (mark:out "    Blocks.Item > nil")
         (progn
           (setq cnt (mark:ax-get rec "Count"))
           (mark:out (strcat "    Entities: " (vl-princ-to-string cnt)))
           (if (and (numberp cnt) (> cnt 0))
             (progn
               (setq i 0)
               (while (< i (min cnt 100))
                 (setq item (mark:ax-invoke rec "Item" i)
                       tn   (if item (mark:ax-get item "ObjectName") nil)
                       en   (if item
                              (vl-catch-all-apply 'vlax-vla-object->ename
                                                  (list item))
                              nil))
                 (if (vl-catch-all-error-p en) (setq en nil))
                 (setq data (if en (entget en) nil))
                 (mark:out
                   (strcat "    [" (itoa i) "] "
                           (vl-princ-to-string tn)
                           (if data
                             (strcat " DXF0="
                                     (vl-princ-to-string
                                       (cdr (assoc 0 data)))
                                     (if (cdr (assoc 2 data))
                                       (strcat " tag/name="
                                               (vl-princ-to-string
                                                 (cdr (assoc 2 data))))
                                       "")
                                     (if (cdr (assoc 1 data))
                                       (strcat " text="
                                               (vl-princ-to-string
                                                 (cdr (assoc 1 data))))
                                       ""))
                             " (нет ename/entget)")))
                 (setq i (1+ i)))))))))))

(defun mark:debug-entnext-chain (e / n data n15)
  (mark:out "  --- entnext после INSERT (66=1) ---")
  (setq n (entnext e)
        n15 0)
  (while (and n (< n15 30))
    (setq data (entget n)
          n15  (1+ n15))
    (if (null data)
      (setq n nil)
      (progn
        (mark:out (strcat "    "
                          (vl-princ-to-string (cdr (assoc 0 data)))
                          (if (cdr (assoc 2 data))
                            (strcat " tag=" (vl-princ-to-string (cdr (assoc 2 data))))
                            "")
                          (if (cdr (assoc 1 data))
                            (strcat " val=" (vl-princ-to-string (cdr (assoc 1 data))))
                            "")))
        (if (mark:name= (cdr (assoc 0 data)) "SEQEND")
          (setq n nil)
          (setq n (entnext n))))))
  (if (= n15 0)
    (mark:out "    (entnext дал другой объект/конец)")))

(defun mark:debug-attr-values (e / ent data tag val)
  (mark:out "  --- Значения ATTRIB (entnext) ---")
  (setq ent (entnext e))
  (while (and ent)
    (setq data (entget ent))
    (cond
      ((null data)
       (setq ent nil))
      ((mark:name= (cdr (assoc 0 data)) "ATTRIB")
       (progn
         (setq tag (cdr (assoc 2 data))
               val (cdr (assoc 1 data)))
         (mark:out (strcat "    " (vl-princ-to-string tag)
                           " = " (vl-princ-to-string val)))
         (setq ent (entnext ent))))
      ((mark:name= (cdr (assoc 0 data)) "SEQEND")
       (setq ent nil))
      (t
       (setq ent nil)))))

(defun mark:debug-one-fill (/ e obj tags tags2)
  (if *mark:fills*
    (progn
      (setq e   (car *mark:fills*)
            obj (mark:vla e))
      (mark:out "  --- Блок заполнения №1 ---")
      (mark:out (strcat "  DXF-2: "
                        (vl-princ-to-string (cdr (assoc 2 (entget e))))))
      (mark:out (strcat "  Name: "
                        (vl-princ-to-string (mark:ax-get obj "Name"))))
      (mark:out (strcat "  EffectiveName: "
                        (vl-princ-to-string (mark:ax-get obj "EffectiveName"))))
      (mark:debug-std-props obj)
      (mark:out (strcat "  INSERT -2: "
                        (vl-princ-to-string (cdr (assoc -2 (entget e))))))
      (setq tags  (mark:attr-tags e)
            tags2 (mark:attr-tags-ax obj))
      (mark:out (strcat "  ATTRIB DXF: " (mark:tags-str tags)))
      (mark:out (strcat "  ATTRIB AX:  " (mark:tags-str tags2)))
      (mark:debug-attr-values e)
      (mark:debug-try-names
        obj
        (list *mark:prop-width*
              *mark:prop-height*
              "Ширина_в_свету"
              "Ширина"
              "Высота"
              "Width"
              "Height"
              "Visibility"
              "Visibility1"
              "Видимость"
              "Марка"
              "Marka"))
      (mark:debug-enum-dyn obj)
      (mark:debug-entnext-chain e)
      (mark:debug-entget e)
      (mark:debug-xdata e)
      (mark:debug-block-def (mark:ax-get obj "EffectiveName")))))

(defun mark:debug-one-glazing (/ e obj tags tags2)
  (if *mark:glazings*
    (progn
      (setq e   (car *mark:glazings*)
            obj (mark:vla e))
      (mark:out "  --- Атрибуты витража ---")
      (mark:out (strcat "  DXF-2: "
                        (vl-princ-to-string (cdr (assoc 2 (entget e))))))
      (mark:out (strcat "  EffectiveName: "
                        (vl-princ-to-string (mark:ax-get obj "EffectiveName"))))
      (mark:debug-std-props obj)
      (mark:out (strcat "  INSERT -2: "
                        (vl-princ-to-string (cdr (assoc -2 (entget e))))))
      (setq tags  (mark:attr-tags e)
            tags2 (mark:attr-tags-ax obj))
      (mark:out (strcat "  ATTRIB DXF: " (mark:tags-str tags)))
      (mark:out (strcat "  ATTRIB AX:  " (mark:tags-str tags2)))
      (mark:debug-try-names obj (list "Витраж" "Vitrage" "Марка" "Visibility"))
      (mark:debug-enum-dyn obj)
      (mark:debug-entnext-chain e)
      (mark:debug-entget e)
      (mark:debug-xdata e)
      (mark:debug-block-def (mark:ax-get obj "EffectiveName")))))

(defun mark:debug-structure ()
  (mark:out "")
  (mark:out "[DEBUG] Разбор структуры (только чтение)...")
  (mark:debug-one-fill)
  (mark:debug-one-glazing))

;;;--------------------- Диагностика (безопасный запуск) ---------------

(defun mark:safe (fn label / r)
  (setq r (vl-catch-all-apply fn nil))
  (if (vl-catch-all-error-p r)
    (progn
      (mark:out (strcat "[ERROR] Сбой на этапе " label ":"))
      (mark:out (strcat "       " (vl-catch-all-error-message r)))
      (mark:note-error)
      nil)
    r))

(defun mark:diagnostics ()
  (mark:safe 'mark:test-parens             "TEST 00")
  (mark:safe 'mark:test-fillings            "TEST 01")
  (mark:safe 'mark:test-glazing-attr        "TEST 02/03")
  (if *mark:records*
    (progn
      (mark:safe 'mark:test-dynamic-properties "TEST 04/05/06")
      (mark:safe 'mark:test-visibility         "TEST 07")
      (mark:safe 'mark:test-mark-attribute     "TEST 08")
      (mark:safe 'mark:test-data               "TEST 09/10/11")
      (mark:safe 'mark:test-specials           "TEST 12")
      (if (> *mark:errors* 0)
        (mark:safe 'mark:debug-structure        "DEBUG")))
    (progn
      (mark:out "")
      (mark:out "[TEST 04] \"Ширина в свету\" — SKIPPED (нет блоков заполнений)")
      (mark:out "[TEST 05] \"Высота в свету\" — SKIPPED (нет блоков заполнений)")
      (mark:out "[TEST 06] Значения размеров — SKIPPED (нет блоков заполнений)")
      (mark:out "[TEST 07] Visibility State — SKIPPED (нет блоков заполнений)")
      (mark:out "[TEST 08] Атрибут \"Марка\" — SKIPPED (нет блоков заполнений)")
      (mark:out "[TEST 09] Уникальные размеры — SKIPPED (нет блоков заполнений)")
      (mark:out "[TEST 10] Буквенная маркировка — SKIPPED (нет блоков заполнений)")
      (mark:out "[TEST 11] Нумерация ширин — SKIPPED (нет блоков заполнений)")
      (mark:out "[TEST 12] Специальные состояния — SKIPPED (нет блоков заполнений)"))))

;;;--------------------- Этап B — запись атрибутов ---------------------

(defun mark:apply-impl (/ cnt failed r e obj aobj aent newm old)
  (setq cnt    0
        failed 0)
  (foreach r *mark:records*
    (setq newm (mark:rec-get r 'mark-new))
    (if newm
      (progn
        (setq e    (mark:rec-get r 'ename)
              obj  (mark:vla e)
              aent (mark:find-attr-in-ename e *mark:attr-mark*)
              aobj (if aent
                     nil
                     (mark:find-attr-obj obj *mark:attr-mark*)))
          (cond
            ;; запись через DXF (entmod) — без ActiveX
            (aent
             (progn
               (setq old (cdr (assoc 1 (entget aent))))
               (if (or (null old) (not (mark:strp old)) (/= old newm))
                 (progn
                   (entmod (subst (cons 1 newm) (assoc 1 (entget aent))
                                  (entget aent)))
                   (entupd aent)))
               (setq cnt (1+ cnt))))
            ;; фолбэк ActiveX
            (aobj
             (progn
               (setq old (mark:ax-get aobj "TextString"))
               (if (or (null old) (not (mark:strp old)) (/= old newm))
                 (mark:ax-put aobj "TextString" newm))
               (setq cnt (1+ cnt))))
            (t
             (setq failed (1+ failed)))))))
  (setq *mark:written*  cnt
        *mark:apply-failed* failed)
  cnt)

(defun mark:apply-marks (/ doc r1 r2)
  (mark:out "")
  (mark:out "[INFO] Начинается запись атрибутов...")
  (setq doc (mark:ax-get (vlax-get-acad-object) "ActiveDocument"))
  ;; В МАРКА внешний маркер открыт — свой не открываем
  (if (and doc (not *mark:batch-undo*))
    (mark:ax-invoke-ok doc "StartUndoMark" nil))
  ;; EndUndoMark закрывается всегда — один шаг UNDO отменяет сеанс записи
  (setq r1 (vl-catch-all-apply 'mark:apply-impl nil))
  (setq r2 (if (and doc (not *mark:batch-undo*))
             (mark:ax-invoke-ok doc "EndUndoMark" nil)
             t))
  (cond
    ((vl-catch-all-error-p r1)
     (progn
       (mark:out "[ERROR] Сбой при записи атрибутов:")
       (mark:out (strcat "       " (vl-catch-all-error-message r1)))
       (mark:note-error)
       (mark:out (strcat "[INFO] Выполнено записей: " (itoa *mark:written*)))
       (mark:out "[INFO] При необходимости выполните UNDO (один шаг).")
       nil))
    ((null r2)
     (progn
       (mark:out "[WARN] Не удалось закрыть Undo-маркер.")
       (mark:note-warning)
       r1))
    (t
     (progn
       (if (> *mark:apply-failed* 0)
         (progn
           (mark:out
             (strcat "[ERROR] Не удалось записать: "
                     (itoa *mark:apply-failed*)))
           (mark:note-error)))
       r1))))

;;;--------------------- Итоговый отчёт --------------------------------

(defun mark:report (success)
  (mark:out "")
  (mark:out "========================================")
  (mark:out (strcat " ИТОГ  " *mark:rev*))
  (mark:out "========================================")
  (mark:out (mark:pad-line "Выбрано объектов:" (itoa *mark:sel-total*)))
  (mark:out
    (mark:pad-line "Заполнений найдено:" (itoa (length *mark:fills*))))
  (mark:out
    (mark:pad-line "\"Атрибуты витража\":"
                   (itoa (length *mark:glazings*))))
  (mark:out
    (mark:pad-line "Витраж:"
                   (if *mark:prefix-found* *mark:prefix* "—")))
  (mark:out
    (mark:pad-line "Уникальных ширин:" (itoa (length *mark:widths*))))
  (mark:out
    (mark:pad-line "Уникальных высот:" (itoa (length *mark:heights*))))
  (mark:out (mark:pad-line "Стемалит:" (itoa *mark:cnt-stem*)))
  (mark:out (mark:pad-line "Сэндвич:"  (itoa *mark:cnt-sand*)))
  (mark:out
    (mark:pad-line "Марки успешно записаны:" (itoa *mark:written*)))
  (if (> *mark:apply-failed* 0)
    (mark:out
      (mark:pad-line "Ошибок записи:" (itoa *mark:apply-failed*))))
  (mark:out (mark:pad-line "Ошибок:" (itoa *mark:errors*)))
  (mark:out (mark:pad-line "Предупреждений:" (itoa *mark:warnings*)))
  (mark:out "========================================")
  (if success
    (mark:out "[INFO] Готово.")
    (progn
      (mark:out "[INFO] Маркировка отменена.")
      (mark:out "[INFO] Изменений в чертеже не выполнено."))))

;;;--------------------- Главная процедура -----------------------------

(defun mark:main (/ r)
  (mark:cmd-line
    "МАРКАРОВКА — марки блоков в «Заполнение в витраж».")
  (mark:reset-state)
  (mark:banner)

  ;; ===== Этап A: подготовка (без изменения чертежа) =====
  (if (null (mark:select))
    (princ)

    (progn
      ;; A1. чтение данных (read-only)
      (setq r (vl-catch-all-apply 'mark:collect-data nil))
      (if (vl-catch-all-error-p r)
        (progn
          (mark:out "[ERROR] Сбой чтения данных блоков:")
          (mark:out (strcat "       " (vl-catch-all-error-message r)))
          (mark:note-error)))

      ;; A2. самодиагностика TEST 01 ... TEST 12
      (mark:diagnostics)

      ;; A3. расчёт марок в памяти + TEST 13
      (mark:out "")
      (cond
        ((= *mark:errors* 0)
         (progn
           (mark:out
             "[INFO] Критических ошибок нет — формирование марок в памяти...")
           (setq r (vl-catch-all-apply 'mark:build-marks nil))
           (if (vl-catch-all-error-p r)
             (progn
               (mark:out "[TEST 13] Проверка формирования марок — ERROR")
               (mark:out (strcat "       " (vl-catch-all-error-message r)))
               (mark:note-error))
             (mark:safe 'mark:validate-marks "TEST 13"))))
        (t
         (progn
           (mark:out "[TEST 13] Проверка формирования марок — SKIPPED")
           (mark:out
             "[INFO] Есть критические ошибки диагностики — марки не формируются."))))

      ;; A3b. диагностика стм/снд vs обычное
      (if (and (= *mark:errors* 0) *mark:records*)
        (vl-catch-all-apply 'mark:diag-base nil))

      ;; A4. финальный контроль
      (mark:pre-check)

      ;; ===== Этап B: применение — только после PRE-CHECK OK =====
      (if *mark:precheck-ok*
        (progn
          (setq r (vl-catch-all-apply 'mark:apply-marks nil))
          (if (or (vl-catch-all-error-p r) (null r))
            (mark:report nil)
            (mark:report t)))
        (mark:report nil))))

  (princ))


;;;=====================================================================
;;;  MARKTABLE — обратное извлечение заполнений в таблицу / XLS / CSV
;;;  Команды: MARKTABLE / МАРКАТАБЛ
;;;  Столбец «Марка» читается из атрибута блока «Заполнение в витраж».
;;;  Размеры: припуск +26 мм (как в ZAPOLNENIE), округление fix().
;;;=====================================================================

;; *mtab:allowance* *mtab:h-keys* *mtab:w-keys* — в шапке

(defun mtab:round2 (x)
  (/ (fix (+ (* x 100.0) 0.5)) 100.0))

(defun mtab:format-area (area / int-part frac)
  (setq int-part (fix area)
        frac (fix (+ (* (- area int-part) 100.0) 0.5)))
  (cond
    ((= frac 0) (itoa int-part))
    ((= (rem frac 10) 0) (strcat (itoa int-part) "," (itoa (/ frac 10))))
    ((< frac 10) (strcat (itoa int-part) ",0" (itoa frac)))
    (t (strcat (itoa int-part) "," (itoa frac)))))

;; Поиск размера: список ключей через mark:get-dyn
(defun mtab:dim (obj keys / k r n)
  (setq r nil)
  (foreach k keys
    (if (null r)
      (progn
        (setq n (mark:get-dyn obj k))
        (if (car n)
          (progn
            (setq n (mark:numval (cadr n)))
            (if (and (numberp n) (> n 0.0))
              (setq r n)))))))
  r)

(defun mtab:select (/ ss i e fills glazings others names nm lst)
  (if (and *mark:reuse-sel* *mark:fills*)
    (progn
      (mark:out "[INFO] Выбор уже выполнен (МАРКА) — без повторного выделения.")
      t)
    (progn
  (mark:out "")
  (prompt "\nВыберите блоки «Заполнение в витраж» для ведомости: ")
  (setq ss (vl-catch-all-apply 'ssget nil))
  (cond
    ((vl-catch-all-error-p ss)
     (mark:out "[ERROR] Ошибка выделения.") nil)
    ((null ss)
     (mark:out "[INFO] Выбор отменён.") nil)
    (t
     (setq fills nil glazings nil others 0 names nil i (sslength ss))
     (repeat i
       (setq i (1- i)
             e (ssname ss i))
       (setq lst (mark:block-names e))
       (foreach nm lst
         (if (not (member nm names))
           (setq names (cons nm names))))
       (cond
         ((mark:blk-match? e *mark:block-fill*)
          (setq fills (cons e fills)))
         ((mark:blk-match? e *mark:block-glazing*)
          (setq glazings (cons e glazings)))
         (t (setq others (1+ others)))))
     (setq *mark:sel-total*  (sslength ss)
           *mark:fills*       (reverse fills)
           *mark:glazings*    (reverse glazings)
           *mark:others*      others
           *mark:found-names* (reverse names))
     (mark:out (strcat "[INFO] Заполнений: " (itoa (length *mark:fills*))))
     (mark:out (strcat "[INFO] \"Атрибуты витража\": " (itoa (length *mark:glazings*))))
     (if (null *mark:fills*)
       (progn (mark:out "[ERROR] Блоки заполнений не найдены.") nil)
       t))))))

;; Префикс Витраж (как в MARKZ)
(defun mtab:prefix (/ g res val)
  (setq *mark:prefix* ""
        *mark:prefix-found* nil)
  (if *mark:glazings*
    (progn
      (setq g (car *mark:glazings*)
            res (mark:find-attr g *mark:attr-vitrage*))
      (if (and (car res) (mark:strp (cadr res)) (/= (mark:trim (cadr res)) ""))
        (progn
          (setq *mark:prefix* (mark:trim (cadr res))
                *mark:prefix-found* t)
          (mark:out (strcat "[INFO] Витраж: " *mark:prefix*)))
        (mark:out "[INFO] Атрибут \"Витраж\" пуст — без префикса.")))
    (mark:out "[INFO] \"Атрибуты витража\" нет — без префикса."))
  *mark:prefix*)

;; Сбор по блокам: (type h w mark)
(defun mtab:collect-blocks (/ blocks e obj raw-h raw-w h w vv vis mres mval)
  (setq blocks nil)
  (foreach e *mark:fills*
    (setq obj (mark:vla e)
          raw-h (mtab:dim obj *mtab:h-keys*)
          raw-w (mtab:dim obj *mtab:w-keys*))
    (cond
      ((or (null raw-h) (null raw-w) (<= raw-h 0.0) (<= raw-w 0.0))
       (mark:out "[WARN] Пропуск блока: нет корректных размеров."))
      (t
       (progn
         (setq h   (fix (+ raw-h *mtab:allowance*))
               w   (fix (+ raw-w *mtab:allowance*))
               vv  (mark:get-visibility obj)
               vis (if vv (caddr vv) "")
               mres (mark:find-attr e *mark:attr-mark*)
               mval (if (car mres) (mark:trim (cadr mres)) ""))
         (if (or (null vis) (= vis ""))
           (setq vis "Без типа"))
         (setq blocks
           (cons (list vis h w mval) blocks))))))
  (reverse blocks))

;; Сортировка: Тип -> Высота -> Ширина -> Марка
(defun mtab:sort-less (a b / ta tb ma mb)
  (setq ta (strcase (car a)) tb (strcase (car b))
        ma (strcase (cadddr a)) mb (strcase (cadddr b)))
  (cond
    ((< ta tb) t)
    ((> ta tb) nil)
    ((< (cadr a) (cadr b)) t)
    ((> (cadr a) (cadr b)) nil)
    ((< (caddr a) (caddr b)) t)
    ((> (caddr a) (caddr b)) nil)
    ((< ma mb) t)
    ((> ma mb) nil)
    (t nil)))

;; Агрегация (type h w mark) -> (type h w mark count)
(defun mtab:aggregate (blocks / acc rec key found)
  ;; acc: assoc, элемент (key type h w mark count)
  (setq acc nil)
  (foreach rec blocks
    (setq key (strcat (strcase (car rec)) "|"
                      (itoa (cadr rec)) "|"
                      (itoa (caddr rec)) "|"
                      (strcase (cadddr rec))))
    (setq found (assoc key acc))
    (if found
      ;; found = (key type h w mark count); cddddr = (mark count)
      (setq acc
        (subst (list key
                     (cadr found)
                     (caddr found)
                     (cadddr found)
                     (car (cddddr found))
                     (1+ (cadr (cddddr found))))
               found acc))
      (setq acc
        (cons (list key (car rec) (cadr rec)
                    (caddr rec) (cadddr rec) 1)
              acc))))
  ;; нормализация к (type h w mark count)
  (setq acc
    (mapcar
      '(lambda (r)
         (list (cadr r) (caddr r) (cadddr r)
               (car (cddddr r)) (cadr (cddddr r))))
      acc))
  (vl-sort acc 'mtab:sort-less))

;; ---------- ТАБЛИЦА AutoCAD ----------
(defun mtab:area (h w cnt)
  (mtab:round2 (/ (* h w cnt) 1000000.0)))

;; ---------- ТАБЛИЦА AutoCAD ----------
;; Порядок: № | Тип | Марка | Высота | Ширина | Кол-во | Площадь
;; Подитог по каждому типу + финальный итог
;; Объединение ячеек строки: столбцы 2-5 -> col 1..4
;; ActiveX: MergeCells(minRow, maxRow, minCol, maxCol)
(defun mtab:merge-2-5 (tbl r / res)
  (setq res
    (vl-catch-all-apply 'vla-MergeCells (list tbl r r 1 4)))
  (if (vl-catch-all-error-p res)
    (progn
      (setq res
        (vl-catch-all-apply 'vlax-invoke-method
                            (list tbl "MergeCells" r r 1 4)))
      (if (vl-catch-all-error-p res)
        (mark:out (strcat "[WARN] MergeCells: "
                          (vl-catch-all-error-message res))))))
  res)

;; ---------- ТАБЛИЦА AutoCAD ----------
;; Порядок: № | Тип | Марка | Высота | Ширина | Кол-во | Площадь
;; Подитог по типу (без слова «Подитог») + финальный итог
(defun mtab:create-table (data / pt doc space tbl row nRows nCols
    rec tip h w mark cnt area oldEcho i n-types
    cur cnt-sub area-sub n-row)
  (if (null data)
    (progn (mark:out "[INFO] Нет данных для таблицы.") nil)
    (progn
      (setq pt (getpoint "\nУкажите точку вставки таблицы: "))
      (if (null pt)
        (progn (mark:out "[INFO] Таблица пропущена.") nil)
        (progn
          (setq n-types 0
                cur nil)
          (foreach rec data
            (if (not (and cur (= (strcase (car rec)) (strcase cur))))
              (progn (setq n-types (1+ n-types)
                           cur (car rec)))))
          (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
                space (vla-get-ModelSpace doc)
                pt (trans pt 1 0)
                nCols 7
                ;; title + header + data + subtotals + total (без лишней строки)
                nRows (+ 3 (length data) n-types)
                oldEcho (getvar "CMDECHO"))
          (vl-catch-all-apply 'setvar (list "CMDECHO" 0))
          (if (not *mark:batch-undo*)
            (mark:ax-invoke-ok doc "StartUndoMark" nil))
          (setq tbl (vl-catch-all-apply 'vla-AddTable
            (list space (vlax-3d-point pt) nRows nCols 10.0 30.0)))
          (if (vl-catch-all-error-p tbl)
            (progn
              (mark:out (strcat "[ERROR] AddTable: "
                                (vl-catch-all-error-message tbl)))
              (if (not *mark:batch-undo*)
                (mark:ax-invoke-ok doc "EndUndoMark" nil))
              (vl-catch-all-apply 'setvar (list "CMDECHO" oldEcho))
              nil)
            (progn
              ;; ширины колонок: № | Тип | Марка | H | W | Кол-во | Площадь
              (vl-catch-all-apply 'vla-SetColumnWidth (list tbl 0 14.0))
              (vl-catch-all-apply 'vla-SetColumnWidth (list tbl 1 45.0))
              (vl-catch-all-apply 'vla-SetColumnWidth (list tbl 2 40.0))
              (vl-catch-all-apply 'vla-SetColumnWidth (list tbl 3 28.0))
              (vl-catch-all-apply 'vla-SetColumnWidth (list tbl 4 28.0))
              (vl-catch-all-apply 'vla-SetColumnWidth (list tbl 5 28.0))
              (vl-catch-all-apply 'vla-SetColumnWidth (list tbl 6 30.0))
              ;; title
              (vla-SetText tbl 0 0 "Заполнение")
              (vl-catch-all-apply 'vla-SetRowHeight (list tbl 0 8.0))
              ;; header
              (setq i 0)
              (foreach hdr '("№" "Тип" "Марка" "Высота, мм" "Ширина, мм"
                             "Кол-во, шт." "Площадь, м2")
                (vla-SetText tbl 1 i hdr)
                (vla-SetCellAlignment tbl 1 i 5)
                (setq i (1+ i)))
              (vl-catch-all-apply 'vla-SetRowHeight (list tbl 1 8.0))
              (setq row 2 n-row 1
                    cur nil
                    cnt-sub 0 area-sub 0.0)
              (foreach rec data
                (setq tip  (car rec)
                      h    (cadr rec)
                      w    (caddr rec)
                      mark (cadddr rec)
                      cnt  (car (cddddr rec))
                      area (mtab:area h w cnt))
                ;; смена типа > подитог предыдущего
                (if (and cur (/= (strcase tip) (strcase cur)))
                  (progn
                    (vla-SetText tbl row 0 "")
                    ;; 3 пробела + название типа, влево, без «Подитог»
                    (vla-SetText tbl row 1 (strcat "   " cur))
                    (mtab:merge-2-5 tbl row)
                    (vla-SetCellAlignment tbl row 1 4)
                    (vla-SetText tbl row 5 (itoa cnt-sub))
                    (vla-SetText tbl row 6 (mtab:format-area area-sub))
                    (vla-SetCellAlignment tbl row 5 5)
                    (vla-SetCellAlignment tbl row 6 5)
                    ;; высота как у строк данных
                    (vl-catch-all-apply 'vla-SetRowHeight (list tbl row 8.0))
                    (setq row (1+ row)
                          cnt-sub 0 area-sub 0.0)))
                (setq cur tip
                      cnt-sub (+ cnt-sub cnt)
                      area-sub (+ area-sub area))
                (vla-SetText tbl row 0 (itoa n-row))
                (vla-SetText tbl row 1 tip)
                (vla-SetText tbl row 2 (if (= mark "") "—" mark))
                (vla-SetText tbl row 3 (itoa h))
                (vla-SetText tbl row 4 (itoa w))
                (vla-SetText tbl row 5 (itoa cnt))
                (vla-SetText tbl row 6 (mtab:format-area area))
                (vla-SetCellAlignment tbl row 0 5)
                (vla-SetCellAlignment tbl row 1 4)
                (vla-SetCellAlignment tbl row 2 4)
                (vla-SetCellAlignment tbl row 3 5)
                (vla-SetCellAlignment tbl row 4 5)
                (vla-SetCellAlignment tbl row 5 5)
                (vla-SetCellAlignment tbl row 6 5)
                (vl-catch-all-apply 'vla-SetRowHeight (list tbl row 8.0))
                (setq row (1+ row)
                      n-row (1+ n-row)))
              ;; последний подитог
              (if cur
                (progn
                  (vla-SetText tbl row 0 "")
                  (vla-SetText tbl row 1 (strcat "   " cur))
                  (mtab:merge-2-5 tbl row)
                  (vla-SetCellAlignment tbl row 1 4)
                  (vla-SetText tbl row 5 (itoa cnt-sub))
                  (vla-SetText tbl row 6 (mtab:format-area area-sub))
                  (vla-SetCellAlignment tbl row 5 5)
                  (vla-SetCellAlignment tbl row 6 5)
                  (vl-catch-all-apply 'vla-SetRowHeight (list tbl row 8.0))
                  (setq row (1+ row))))
              ;; финальный итог
              (setq cnt-sub 0 area-sub 0.0)
              (foreach rec data
                (setq h (cadr rec) w (caddr rec)
                      cnt (car (cddddr rec))
                      cnt-sub (+ cnt-sub cnt)
                      area-sub (+ area-sub (mtab:area h w cnt))))
              (vla-SetText tbl row 0 "")
              ;; 3 пробела + «Итого:», подчёркивание, влево
              (vla-SetText tbl row 1 "   {\\LИтого:}")
              (mtab:merge-2-5 tbl row)
              (vla-SetCellAlignment tbl row 1 4)
              (vla-SetText tbl row 5 (itoa cnt-sub))
              (vla-SetText tbl row 6 (mtab:format-area area-sub))
              (vla-SetCellAlignment tbl row 5 5)
              (vla-SetCellAlignment tbl row 6 5)
              (vl-catch-all-apply 'vla-SetRowHeight (list tbl row 8.0))
              (vl-catch-all-apply 'vla-Update (list tbl))
              (if (not *mark:batch-undo*)
                (mark:ax-invoke-ok doc "EndUndoMark" nil))
              (vl-catch-all-apply 'setvar (list "CMDECHO" oldEcho))
              (mark:out (strcat "[INFO] Таблица создана, строк данных: "
                                (itoa (length data))))
              t)))))))

;; ---------- XML escape / CSV quote ----------
(defun mtab:xml-escape (str / result ch)
  (setq result "")
  (if (null str) (setq str ""))
  (while (/= str "")
    (setq ch (substr str 1 1))
    (cond
      ((= ch "&") (setq result (strcat result "&amp;")))
      ((= ch "<") (setq result (strcat result "&lt;")))
      ((= ch ">") (setq result (strcat result "&gt;")))
      ((= ch "\"") (setq result (strcat result "&quot;")))
      ((= ch "'") (setq result (strcat result "&apos;")))
      (t (setq result (strcat result ch))))
    (setq str (substr str 2)))
  result)

(defun mtab:csv-quote (str)
  (if (null str) ""
    (if (or (vl-string-search ";" str)
            (vl-string-search "\"" str))
      (strcat "\"" (vl-string-subst "\"\"" "\"" str) "\"")
      str)))

;; ---------- XLS (XML Spreadsheet 2003) ----------
;; ТЗ: колонка площади и итоги — ФОРМУЛЫ Excel (не зашитые числа).
;; Примитив mtab:cell (аналог eu-cell): ss:Formula + XML-экранирование.
;; Синтаксис: ROUND(...,2) / SUM(...), английские имена, точка — дробь, запятая — разделитель.
;; A1: счёт строк/колонок ведётся здесь же; на ss:Index в формулах не полагаемся.

(defun mtab:rtos-xml (x / s)
  ;; число для SpreadsheetML: разделитель — точка
  (setq s (rtos (float x) 2 2))
  (while (vl-string-search "," s)
    (setq s (vl-string-subst "." "," s)))
  s)

;; Примитив записи ячейки (аналог eu-cell из excel-utils.lsp)
;; formula = nil > без ss:Formula; иначе — каноническая формула A1
(defun mtab:cell (f style formula dtype val)
  (if formula
    (write-line
      (strcat "    <Cell ss:StyleID=\"" style
              "\" ss:Formula=\"" (mtab:xml-escape formula) "\">"
              "<Data ss:Type=\"" dtype "\">"
              (mtab:xml-escape val)
              "</Data></Cell>")
      f)
    (write-line
      (strcat "    <Cell ss:StyleID=\"" style "\">"
              "<Data ss:Type=\"" dtype "\">"
              (mtab:xml-escape val)
              "</Data></Cell>")
      f)))

;; Ячейка с объединением столбцов (MergeAcross) + опциональная формула
(defun mtab:cell-m (f style merge-across formula dtype val)
  (if formula
    (write-line
      (strcat "    <Cell ss:StyleID=\"" style
              "\" ss:MergeAcross=\"" (itoa merge-across)
              "\" ss:Formula=\"" (mtab:xml-escape formula) "\">"
              "<Data ss:Type=\"" dtype "\">"
              (mtab:xml-escape val)
              "</Data></Cell>")
      f)
    (write-line
      (strcat "    <Cell ss:StyleID=\"" style
              "\" ss:MergeAcross=\"" (itoa merge-across) "\">"
              "<Data ss:Type=\"" dtype "\">"
              (mtab:xml-escape val)
              "</Data></Cell>")
      f)))

;; маркер идемпотентности патча формул
;; mtab:eu-cell-formula

;; Формула SUM в R1C1 для диапазона строк r0..r1 в ТЕКУЩЕЙ колонке
;; (SpreadsheetML — каноничный R1C1; одна строка > прямая ссылка R[n]C)
(defun mtab:sum-r1c1 (r0 r1 / n0)
  (setq n0 (- r0 r1))  ;; отрицательный сдвиг от строки формулы (r1 = строка над формулой)
  ;; формула пишется в строку (r1+1), сдвиг до r0 = r0-(r1+1) = n0-1
  (setq n0 (1- n0))
  (if (= n0 -1)
    (strcat "=R[" (itoa -1) "]C")
    (strcat "=SUM(R[" (itoa n0) "]C:R[" (itoa -1) "]C)")))

;; =SUM(R4C6,R6C6,…) — абсолютные строки подитогов, col 6=F / 7=G
(defun mtab:sum-abs-r1c1 (rows col / out r)
  (if (null rows)
    "=0"
    (progn
      (setq out "")
      (foreach r rows
        (setq out
          (if (= out "")
            (strcat "R" (itoa r) "C" (itoa col))
            (strcat out ",R" (itoa r) "C" (itoa col)))))
      (strcat "=SUM(" out ")"))))

(defun mtab:export-xls (data xlsfile / f tip h w mark cnt area
                          total-cnt total-area rec cur
                          cnt-sub area-sub
                          xl-row grp-start sub-rows n-no)
  (setq f (open xlsfile "w"))
  (if (null f)
    (progn (mark:out (strcat "[ERROR] Нет доступа: " xlsfile)) nil)
    (progn
      (write-line "<?xml version=\"1.0\" encoding=\"windows-1251\"?>" f)
      (write-line "<?mso-application progid=\"Excel.Sheet\"?>" f)
      (write-line "<Workbook xmlns=\"urn:schemas-microsoft-com:office:spreadsheet\"" f)
      (write-line " xmlns:o=\"urn:schemas-microsoft-com:office:office\"" f)
      (write-line " xmlns:x=\"urn:schemas-microsoft-com:office:excel\"" f)
      (write-line " xmlns:ss=\"urn:schemas-microsoft-com:office:spreadsheet\"" f)
      (write-line " xmlns:html=\"http://www.w3.org/TR/REC-html40\">" f)
      (write-line " <Styles>" f)
      (write-line "  <Style ss:ID=\"Default\" ss:Name=\"Normal\">" f)
      (write-line "   <Font ss:FontName=\"Calibri\" ss:Size=\"11\"/>" f)
      (write-line "  </Style>" f)
      (write-line "  <Style ss:ID=\"D\">" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)
      (write-line "  <Style ss:ID=\"H\">" f)
      (write-line "   <Font ss:Bold=\"1\"/>" f)
      (write-line "   <Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/>" f)
      (write-line "   <Alignment ss:Horizontal=\"Center\"/>" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)
      ;; TITLE — заголовок «Заполнение»: жирный + подчёркивание
      (write-line "  <Style ss:ID=\"TITLE\">" f)
      (write-line "   <Font ss:Bold=\"1\" ss:Underline=\"Single\"/>" f)
      (write-line "   <Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/>" f)
      (write-line "   <Alignment ss:Horizontal=\"Center\"/>" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)
      ;; S — подитог: подпись влево
      (write-line "  <Style ss:ID=\"S\">" f)
      (write-line "   <Font ss:Bold=\"1\"/>" f)
      (write-line "   <Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/>" f)
      (write-line "   <Alignment ss:Horizontal=\"Left\"/>" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)
      ;; SN — числа подитога вправо
      (write-line "  <Style ss:ID=\"SN\">" f)
      (write-line "   <Font ss:Bold=\"1\"/>" f)
      (write-line "   <Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/>" f)
      (write-line "   <Alignment ss:Horizontal=\"Right\"/>" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)
      ;; T — итог: подпись влево; без Underline на ячейке (пробелы не подчёркивать)
      (write-line "  <Style ss:ID=\"T\">" f)
      (write-line "   <Font ss:Bold=\"1\"/>" f)
      (write-line "   <Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/>" f)
      (write-line "   <Alignment ss:Horizontal=\"Left\"/>" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)
      ;; TN — числа итога вправо
      (write-line "  <Style ss:ID=\"TN\">" f)
      (write-line "   <Font ss:Bold=\"1\"/>" f)
      (write-line "   <Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/>" f)
      (write-line "   <Alignment ss:Horizontal=\"Right\"/>" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)
      (write-line " </Styles>" f)
      (write-line " <Worksheet ss:Name=\"Заполнение\">" f)
      (write-line "  <Table>" f)
      (foreach w0 '("30" "130" "90" "70" "70" "70" "70")
        (write-line (strcat "   <Column ss:Width=\"" w0 "\"/>") f))
      ;; row1 title, row2 header, data from 3
      ;; A № | B Тип | C Марка | D Высота | E Ширина | F Кол-во | G Площадь
      (write-line (strcat "   <Row><Cell ss:StyleID=\"TITLE\" ss:MergeAcross=\"6\">"
                          "<Data ss:Type=\"String\">Заполнение</Data></Cell></Row>") f)
      (write-line "   <Row>" f)
      (foreach hdr '("№" "Тип" "Марка" "Высота, мм" "Ширина, мм"
                      "Кол-во, шт." "Площадь, м2")
        (mtab:cell f "H" nil "String" hdr))
      (write-line "   </Row>" f)
      (setq xl-row 3 total-cnt 0 total-area 0.0
            cur nil cnt-sub 0 area-sub 0.0
            grp-start 3 sub-rows nil
            n-no 0)
      (foreach rec data
        (setq tip  (car rec)
              h    (cadr rec)
              w    (caddr rec)
              mark (cadddr rec)
              cnt  (car (cddddr rec))
              area (mtab:area h w cnt))
        ;; подитог: R1C1 SUM по группе (одна строка > прямая ссылка)
        (if (and cur (/= (strcase tip) (strcase cur)))
          (progn
            (write-line "   <Row>" f)
            (mtab:cell f "S" nil "String" "")
            (mtab:cell-m f "S" 3 nil "String" (strcat "   " cur))
            (mtab:cell f "SN"
                       (mtab:sum-r1c1 grp-start (1- xl-row))
                       "Number" (itoa cnt-sub))
            (mtab:cell f "SN"
                       (mtab:sum-r1c1 grp-start (1- xl-row))
                       "Number" (mtab:rtos-xml area-sub))
            (write-line "   </Row>" f)
            (setq sub-rows (cons xl-row sub-rows)
                  cnt-sub 0 area-sub 0.0
                  xl-row (1+ xl-row)
                  grp-start xl-row)))
        (setq cur tip
              total-cnt (+ total-cnt cnt)
              total-area (+ total-area area)
              cnt-sub (+ cnt-sub cnt)
              area-sub (+ area-sub area))
        (setq n-no (1+ n-no))
        (write-line "   <Row>" f)
        (mtab:cell f "D" nil "Number" (itoa n-no))
        (mtab:cell f "D" nil "String" tip)
        (mtab:cell f "D" nil "String" mark)
        (mtab:cell f "D" nil "Number" (itoa h))
        (mtab:cell f "D" nil "Number" (itoa w))
        (mtab:cell f "D" nil "Number" (itoa cnt))
        ;; площадь — R1C1: D*E*F той же строки / 1e6, округление до 2
        (mtab:cell f "D"
                   "=ROUND(RC[-3]*RC[-2]*RC[-1]/1000000,2)"
                   "Number" (mtab:rtos-xml area))
        (write-line "   </Row>" f)
        (setq xl-row (1+ xl-row)))
      ;; последний подитог
      (if cur
        (progn
          (write-line "   <Row>" f)
          (mtab:cell f "S" nil "String" "")
          (mtab:cell-m f "S" 3 nil "String" (strcat "   " cur))
          (mtab:cell f "SN"
                     (mtab:sum-r1c1 grp-start (1- xl-row))
                     "Number" (itoa cnt-sub))
          (mtab:cell f "SN"
                     (mtab:sum-r1c1 grp-start (1- xl-row))
                     "Number" (mtab:rtos-xml area-sub))
          (write-line "   </Row>" f)
          (setq sub-rows (cons xl-row sub-rows)
                xl-row (1+ xl-row))))
      ;; финальный итог: =SUM(R…C6,…) / =SUM(R…C7,…) по строкам подитогов
      (write-line "   <Row>" f)
      (mtab:cell f "T" nil "String" "")
      ;; 3 пробела + подчёркнутый только «Итого:»
      (write-line
        (strcat "    <Cell ss:StyleID=\"T\" ss:MergeAcross=\"3\">"
                "<Data ss:Type=\"String\">   "
                "<html:B><html:U>Итого:</html:U></html:B></Data></Cell>")
        f)
      (mtab:cell f "TN"
                 (mtab:sum-abs-r1c1 sub-rows 6)
                 "Number" (itoa total-cnt))
      (mtab:cell f "TN"
                 (mtab:sum-abs-r1c1 sub-rows 7)
                 "Number" (mtab:rtos-xml total-area))
      (write-line "   </Row>" f)
      (write-line "  </Table>" f)
      (write-line " </Worksheet>" f)
      (write-line "</Workbook>" f)
      (close f)
      (mark:out (strcat "[INFO] XLS: " xlsfile))
      t)))

;; ---------- CSV ----------
(defun mtab:export-csv (data csvfile / f rec tip h w mark cnt area
                         total-cnt total-area n cur
                         cnt-sub area-sub)
  (setq f (open csvfile "w"))
  (if (null f)
    (progn (mark:out (strcat "[ERROR] Нет доступа: " csvfile)) nil)
    (progn
      (write-line "Заполнение" f)
      (write-line "№;Тип;Марка;Высота, мм;Ширина, мм;Кол-во, шт.;Площадь, м2" f)
      (setq n 1 total-cnt 0 total-area 0.0
            cur nil cnt-sub 0 area-sub 0.0)
      (foreach rec data
        (setq tip  (car rec)
              h    (cadr rec)
              w    (caddr rec)
              mark (cadddr rec)
              cnt  (car (cddddr rec))
              area (mtab:area h w cnt))
        (if (and cur (/= (strcase tip) (strcase cur)))
          (progn
            (write-line
              (strcat ";Подитог: " (mtab:csv-quote cur) ";;;;"
                      (itoa cnt-sub) ";"
                      "\"" (mtab:format-area area-sub) "\"")
              f)
            (setq cnt-sub 0 area-sub 0.0)))
        (setq cur tip
              total-cnt (+ total-cnt cnt)
              total-area (+ total-area area)
              cnt-sub (+ cnt-sub cnt)
              area-sub (+ area-sub area))
        (write-line
          (strcat (itoa n) ";"
                  (mtab:csv-quote tip) ";"
                  (mtab:csv-quote mark) ";"
                  (itoa h) ";"
                  (itoa w) ";"
                  (itoa cnt) ";"
                  "\"" (mtab:format-area area) "\"")
          f)
        (setq n (1+ n)))
      (if cur
        (write-line
          (strcat ";Подитог: " (mtab:csv-quote cur) ";;;;"
                  (itoa cnt-sub) ";"
                  "\"" (mtab:format-area area-sub) "\"")
          f))
      (write-line
        (strcat ";Итого;;;;"
                (itoa total-cnt) ";"
                "\"" (mtab:format-area total-area) "\"")
        f)
      (close f)
      (mark:out (strcat "[INFO] CSV: " csvfile))
      t)))

;; ---------- Основная ----------
(defun mtab:main (/ data base do-table do-xls xls-ok)
  (mark:cmd-line "МАРКАТАБЛ — ведомость по блокам «Заполнение в витраж».")
  (mark:banner)
  (if (null (mtab:select))
    (princ)
    (progn
      (mtab:prefix)
      (setq data (mtab:aggregate (mtab:collect-blocks)))
      (if (null data)
        (mark:out "[ERROR] Нет данных (проверьте размеры блоков).")
        (progn
          (mark:out (strcat "[INFO] Уникальных позиций: " (itoa (length data))))
          (initget "Y N")
          (setq do-table
            (getkword "\nСоздать таблицу AutoCAD? [Да(Y)/Нет(N)] <Y>: "))
          (if (or (null do-table) (= do-table "Y"))
            (setq do-table t) (setq do-table nil))
          (initget "Y N")
          (setq do-xls
            (getkword "\nЭкспорт в XLS? [Да(Y)/Нет(N)] <Y>: "))
          (if (or (null do-xls) (= do-xls "Y"))
            (setq do-xls t) (setq do-xls nil))
          (setq base
            (strcat (getvar "dwgprefix")
                    (vl-filename-base (getvar "dwgname"))
                    " Заполнение Марки"))
          (setq xls-ok nil)
          (if do-xls
            (progn
              (setq xls-ok (mtab:export-xls data (strcat base ".xls")))
              ;; CSV — только резерв, если XLS не создался
              (if (null xls-ok)
                (progn
                  (mark:out "[INFO] XLS не создан — резервный экспорт CSV.")
                  (mtab:export-csv data (strcat base ".csv"))))))
          (if do-table
            (mtab:create-table data))
          (mark:out "[INFO] Готово.")))))
  (princ))

(defun c:MARKTABLE () (mtab:main))
(defun c:МАРКАТАБЛ () (mtab:main))


;;;=====================================================================
;;;  MARKAR — рядовка условных обозначений (Ряд заполнений)
;;;  Команда: МАРКАРЯД
;;;  После МАРКАРОВКА: индекс от размера (буква = высота, номер = ширина).
;;;  Старые «Ряд заполнений» этой зоны удаляются и ставятся заново.
;;;  Горизонталь: номера 1,2,3… снизу на min(y0)-1000, соосно по X.
;;;  Вертикаль: буквы А,Б,В… справа на max(x1)+1000.
;;;  Разная W в столбце > этаж ниже; разная H в ряду > этаж правее.
;;;=====================================================================

;; *mark:ar-* — в шапке

(defun mark:ar-banner ()
  (mark:out "========================================")
  (mark:out (strcat " РЯДОВКА УСЛОВНЫХ ОБОЗНАЧЕНИЙ  "
                    *mark:rev*))
  (mark:out "========================================"))

;;;--- Разбор Марки: [Витраж] + " " + БУКВА + НОМЕР + [суффикс] ---------

;; Суффикс стм/снд не входит в индекс
(defun mark:ar-strip-suffix (m / out msc sp suf n list)
  (setq out (mark:trim (if m m ""))
        msc (strcase out)
        list nil)
  (foreach sp *mark:specials*
    (setq list (cons (strcase (cdr sp)) list)))
  (foreach suf list
    (setq n (strlen suf))
    (if (and (> (strlen msc) n)
             (= (substr msc (1+ (- (strlen msc) n))) suf))
      (setq out (substr out 1 (- (strlen out) n))
            msc (strcase out))))
  out)

(defun mark:ar-tail (m / i n)
  (setq n (strlen m)
        i n)
  (while (and (> i 0) (/= (substr m i 1) " "))
    (setq i (1- i)))
  (if (= i 0) m (substr m (1+ i))))

(defun mark:ar-letter? (ch)
  (and (mark:strp ch)
       (= (strlen ch) 1)
       (wcmatch ch "[А-Яа-я]")))

(defun mark:ar-digit? (ch)
  (and (mark:strp ch)
       (= (strlen ch) 1)
       (>= (ascii ch) 48)
       (<= (ascii ch) 57)))

;; > (letter num) | nil
(defun mark:ar-parse (m / tail i n letter num)
  (setq m (mark:ar-strip-suffix (mark:trim m)))
  (if (or (null m) (= m ""))
    nil
    (progn
      (setq tail  (mark:ar-tail m)
            i     1
            n     (strlen tail)
            letter ""
            num   "")
      (while (and (<= i n)
                  (mark:ar-letter? (substr tail i 1))
                  ;; Й, З не используем; strcase без T = ВЕРХНИЙ регистр
                  (not (member (strcase (substr tail i 1))
                               '("Й" "З"))))
        (setq letter (strcat letter (strcase (substr tail i 1)))
              i      (1+ i)))
      (while (and (<= i n) (mark:ar-digit? (substr tail i 1)))
        (setq num (strcat num (substr tail i 1))
              i   (1+ i)))
      (if (and (/= letter "") (/= num ""))
        (list (strcase letter) num)
        nil))))

;;;--- Сбор: rec = (letter num e cx cy x0 x1 y0 y1 w h) -----------------

(defun mark:ar-collect (fills / out e obj w h wh hh ins mres mval pa
                             x0 y0 x1 y1 cx cy)
  ;; 1) сбор: геометрия + W/H теми же ключами, что в МАРКАРОВКА;
  ;;    разбор Марки — только подсказка, не фильтр
  (setq out nil)
  (foreach e fills
    (setq obj  (mark:vla e)
          wh   (mark:get-dyn obj *mark:prop-width*)
          hh   (mark:get-dyn obj *mark:prop-height*)
          w    (if (and (car wh) (mark:numval (cadr wh)))
                 (mark:numval (cadr wh))
                 (mtab:dim obj *mtab:w-keys*))
          h    (if (and (car hh) (mark:numval (cadr hh)))
                 (mark:numval (cadr hh))
                 (mtab:dim obj *mtab:h-keys*))
          ins  (cdr (assoc 10 (entget e)))
          mres (mark:find-attr e *mark:attr-mark*)
          mval (if (car mres) (cadr mres) "")
          pa   (mark:ar-parse mval))
    (cond
      ((or (null w) (null h) (<= w 0.0) (<= h 0.0))
       (progn
         (mark:out "[WARN] Нет W/H — пропуск.")
         (mark:note-warning)))
      ((null ins)
       (progn
         (mark:out "[WARN] Нет точки вставки — пропуск.")
         (mark:note-warning)))
      (t
       (progn
         ;; сообщения о марке — INFO/WARN, но не выбрасываем блок
         (cond
           ((null (car mres))
            (mark:out
              "[WARN] Нет атрибута \"Марка\" — индекс будет от размера."))
           ((or (null mval) (= (mark:trim mval) ""))
            (mark:out
              "[WARN] Атрибут \"Марка\" пуст — индекс будет от размера."))
           ((null pa)
            (mark:out
              (strcat "[WARN] Марка не разобрана (" mval
                      ") — индекс будет от размера."))))
         (setq x0 (float (car ins))
               y0 (float (cadr ins))
               x1 (+ x0 (float w))
               y1 (+ y0 (float h))
               cx (/ (+ x0 x1) 2.0)
               cy (/ (+ y0 y1) 2.0))
         (setq out
           (cons (list (if pa (car pa) "")
                       (if pa (cadr pa) "")
                       e cx cy x0 x1 y0 y1
                       (float w) (float h))
                 out))))))
  (setq out (reverse out))
  ;; 2) один знаменатель: индекс ТОЛЬКО от W/H — как МАРКАРОВКА
  (mark:ar-reindex out))

;; Переназначение: буква = индекс высоты, номер = индекс ширины + 1
;; rec = (letter num e cx cy x0 x1 y0 y1 w h)
(defun mark:ar-reindex (recs / ws hs ws1 hs1 r w h iw ih letter num out)
  (setq ws1 nil
        hs1 nil)
  (foreach r recs
    (setq w (nth 9 r)
          h (nth 10 r))
    (if (and (numberp w) (> w 0.0))
      (setq ws1 (cons w ws1)))
    (if (and (numberp h) (> h 0.0))
      (setq hs1 (cons h hs1))))
  (setq ws (mark:sort-unique (mark:positives ws1))
        hs (mark:sort-unique (mark:positives hs1))
        *mark:widths*  ws
        *mark:heights* hs
        out nil)
  (foreach r recs
    (setq w     (nth 9 r)
          h     (nth 10 r)
          iw    (mark:index-of ws w)
          ih    (mark:index-of hs h)
          letter (nth 0 r)
          num    (nth 1 r))
    ;; не оставлять букву/номер из старой Марки: индекс только от размера
    (setq letter ""
          num    "")
    (if ih
      (setq letter (mark:letter-at ih)))
    (if (mark:strp letter)
      (setq letter (strcase letter)))
    (if iw
      (setq num (itoa (1+ iw))))
    (setq out
      (cons (list letter num
                  (nth 2 r) (nth 3 r) (nth 4 r)
                  (nth 5 r) (nth 6 r) (nth 7 r) (nth 8 r)
                  w h)
            out)))
  (reverse out))

;;;--- Кластеризация ----------------------------------------------------

(defun mark:ar-clusters (vals tol / s g out v lastv)
  (setq s   (vl-sort (mapcar 'float vals) '<)
        out nil
        g   nil
        lastv nil)
  (foreach v s
    (if (and g (<= (abs (- v lastv)) tol))
      (setq g     (append g (list v))
            lastv v)
      (progn
        (if g (setq out (cons g out)))
        (setq g     (list v)
              lastv v))))
  (if g (setq out (cons g out)))
  (reverse out))

;; recs > ((cx0 rec...) (cx1 rec...) ...) по возрастанию ключа
;; idx — индекс числового поля в rec (2=cx нет, см. вызовы)
(defun mark:ar-group-idx (recs idx tol / keys clus out g r hit m)
  (setq keys nil)
  (foreach r recs
    (setq keys (cons (nth idx r) keys)))
  (setq clus (mark:ar-clusters keys tol)
        out  nil)
  (foreach g clus
    (setq m   (car g)
          hit nil)
    (foreach r recs
      (if (<= (abs (- (float (nth idx r)) (float m))) tol)
        (setq hit (cons r hit))))
    ;; порядок recs: как в исходном списке
    (setq hit (reverse hit))
    ;; в группе сортируем по idx (стабильно для кластера)
    (setq hit (vl-sort hit
                '(lambda (a b)
                   (< (nth idx a) (nth idx b)))))
    (if hit (setq out (cons hit out))))
  (reverse out))

;; внутри уже отсортированного по idx списка — подгруппы по другому idx
(defun mark:ar-subgroup (recs idx2 tol / keys clus out g r hit m)
  (setq keys nil)
  (foreach r recs
    (setq keys (cons (nth idx2 r) keys)))
  (setq clus (mark:ar-clusters keys tol)
        out  nil)
  (foreach g clus
    (setq m   (car g)
          hit nil)
    (foreach r recs
      (if (<= (abs (- (float (nth idx2 r)) (float m))) tol)
        (setq hit (cons r hit))))
    (setq hit (reverse hit))
    (if hit (setq out (cons hit out))))
  ;; порядок подгрупп: по ключу (clusters уже по возрастанию)
  out)

(defun mark:ar-sort-by (groups idx-desc / out g)
  ;; сортировка групп по (nth idx (car group)) по убыванию
  (setq out
    (vl-sort groups
      '(lambda (a b)
         (> (nth idx-desc (car a))
            (nth idx-desc (car b))))))
  out)

;;;--- Уже размещённые «Ряд заполнений» ---------------------------------

(defun mark:ar-exists (x y lst / p hit)
  (setq p nil)
  (foreach e lst
    (if (and (null p)
             (<= (distance (list x y 0.0)
                           (list (car e) (cadr e) 0.0))
                 *mark:ar-tol*))
      (setq p e)))
  p)

;;;--- Снять старую рядовку в зоне этой раскладки ----------------------
;; Группу удаляем отдельно: vla-Delete группы не удаляет вставки.
;; Полоса снизу и полоса справа — те же места, куда МАРКАРЯД ставит ряды.
(defun mark:ar-erase-old (recs / ss i e n obj p x y
                              x0 y0 x1 y1 r victims
                              xmin xmax ymin ymax
                              yb xb margin doc groups old name)
  (setq xmin nil xmax nil ymin nil ymax nil n 0 victims nil)
  (foreach r recs
    (setq x0 (nth 5 r)
          y0 (nth 7 r)
          x1 (nth 6 r)
          y1 (nth 8 r))
    (if (or (null xmin) (< x0 xmin)) (setq xmin x0))
    (if (or (null xmax) (> x1 xmax)) (setq xmax x1))
    (if (or (null ymin) (< y0 ymin)) (setq ymin y0))
    (if (or (null ymax) (> y1 ymax)) (setq ymax y1)))
  (setq doc    (mark:ax-get (vlax-get-acad-object) "ActiveDocument")
        groups (if doc (mark:ax-get doc "Groups") nil))
  (if groups
    (foreach name (list *mark:ar-group-h* *mark:ar-group-v*)
      (setq old (mark:ax-invoke groups "Item" name))
      (if old
        (vl-catch-all-apply 'vla-Delete (list old)))))
  (if (and xmin xmax ymin ymax)
    (progn
      (setq margin 500.0
            yb     (- ymin *mark:ar-offset*)
            xb     (+ xmax *mark:ar-offset*)
            ss     (vl-catch-all-apply 'ssget
                     (list "X" (list (cons 0 "INSERT")))))
      (if (and ss (not (vl-catch-all-error-p ss)))
        (progn
          (setq i (sslength ss))
          (repeat i
            (setq i (1- i)
                  e (ssname ss i))
            (if (mark:blk-match? e *mark:ar-block*)
              (setq victims (cons e victims))))
          (foreach e victims
            (setq p (cdr (assoc 10 (entget e))))
            (if p
              (progn
                (setq x (float (car p))
                      y (float (cadr p)))
                (if (or (and (<= y yb)
                             (>= y (- yb (* 40.0 *mark:ar-line*)))
                             (>= x (- xmin margin))
                             (<= x (+ xmax margin)))
                        (and (>= x xb)
                             (<= x (+ xb (* 40.0 *mark:ar-line*)))
                             (>= y (- ymin margin))
                             (<= y (+ ymax margin))))
                  (progn
                    (setq obj (mark:vla e))
                    (if obj
                      (vl-catch-all-apply 'vla-Delete (list obj))
                      (vl-catch-all-apply 'entdel (list e)))
                    (setq n (1+ n)))))))))))
  (if (> n 0)
    (mark:out
      (strcat "[INFO] Удалена старая рядовка: " (itoa n) " блок(ов).")))
  n)

(defun mark:ar-placed-pts (/ ss i e lst p)
  (setq lst nil)
  (setq ss (vl-catch-all-apply 'ssget
             (list "X" (list (cons 0 "INSERT")))))
  (if (and ss (not (vl-catch-all-error-p ss)))
    (progn
      (setq i (sslength ss))
      (repeat i
        (setq i (1- i)
              e (ssname ss i))
        (if (mark:blk-match? e *mark:ar-block*)
          (progn
            (setq p (cdr (assoc 10 (entget e))))
            (if p
              (setq lst (cons (list (float (car p))
                                    (float (cadr p)))
                              lst))))))))
  lst)

;;;--- Вставка -----------------------------------------------------------

(defun mark:ar-set-attr (obj tag val / atts att cnt i tagv ok aent e
                            ent data)
  ;; 1) ActiveX: Attributes / GetAttributes
  (setq ok   nil
        atts (mark:ax-get obj "Attributes"))
  (if (null atts)
    (setq atts (mark:ax-invoke obj "GetAttributes" nil)))
  (if atts
    (progn
      (setq cnt (mark:ax-get atts "Count")
            i   0)
      (if (and (numberp cnt) (> cnt 0))
        (while (< i cnt)
          (setq att  (mark:ax-invoke atts "Item" i)
                tagv (if att (mark:ax-get att "TagString") nil))
          (if (and tagv (mark:name= tagv tag))
            (progn
              (if (mark:ax-put att "TextString" val)
                (setq ok t))
              (setq i cnt)))
          (setq i (1+ i))))))
  ;; 2) fallback: entmod по ename
  (if (null ok)
    (progn
      (setq e (vl-catch-all-apply 'vlax-vla-object->ename (list obj)))
      (if (and (not (vl-catch-all-error-p e)) e)
        (progn
          (setq aent (mark:find-attr-in-ename e tag))
          (if aent
            (progn
              (setq data (entget aent))
              (entmod (subst (cons 1 val) (assoc 1 data) data))
              (entupd aent)
              (setq ok t)))))))
  ;; 3) fallback: ActiveX-атрибут через find-attr-obj
  (if (null ok)
    (progn
      (setq att (mark:find-attr-obj obj tag))
      (if (and att (mark:ax-put att "TextString" val))
        (setq ok t))))
  ok)

;; Запись dynamic-свойства: GetDynamicBlockProperties > PropertyNumberValue
(defun mark:ar-put-dim (obj keys val / r v lst item nm ok k)
  (setq ok nil)
  (setq r (vl-catch-all-apply 'vlax-invoke-method
            (list obj "GetDynamicBlockProperties")))
  (if (and (not (vl-catch-all-error-p r)) r)
    (progn
      (setq v   (mark:unwrap r)
            lst (vl-catch-all-apply 'vlax-safearray->list (list v)))
      (if (and (not (vl-catch-all-error-p lst)) (listp lst))
        (foreach item lst
          (setq item (mark:unwrap item)
                nm   (mark:dyn-item-name item))
          (if (and (null ok) (mark:strp nm))
            (progn
              (setq k nil)
              (foreach key keys
                (if (and (null k) (mark:name= nm key))
                  (setq k key)))
              (if k
                (cond
                  ((mark:ax-put item "PropertyNumberValue" (float val))
                   (setq ok t))
                  ((mark:ax-put item "Value" (float val))
                   (setq ok t))
                  ((mark:ax-put item "PropertyValue" (float val))
                   (setq ok t))))))))))
  ;; фолбэк: прямая запись на объект
  (if (null ok)
    (foreach k keys
      (if (null ok)
        (if (mark:ax-put obj k (float val))
          (setq ok t)))))
  ok)

;; rot — в ГРАДУСАХ (270 — горизонтальный ряд, 0 — вертикаль)
(defun mark:ar-insert (space x y txt rot / obj)
  (setq obj
    (vl-catch-all-apply 'vla-InsertBlock
      (list space
            (vlax-3d-point (list (float x) (float y) 0.0))
            *mark:ar-block*
            1.0 1.0 1.0
            ;; InsertBlock принимает РАДИАНЫ
            (* (float rot) (/ pi 180.0)))))
  (cond
    ((vl-catch-all-error-p obj)
     (progn
       (mark:out (strcat "[ERROR] INSERT: "
                         (vl-catch-all-error-message obj)))
       (mark:note-error)
       nil))
    ((null obj)
     (progn
       (mark:out "[ERROR] INSERT nil.")
       (mark:note-error)
       nil))
    (t
     (progn
       ;; страховка: выставить Rotation в градусах через свойство
       (mark:ax-put obj "Rotation" (* (float rot) (/ pi 180.0)))
       (if (not (mark:ar-set-attr obj *mark:ar-attr* txt))
         (progn
           (mark:out
             (strcat "[WARN] Не записан атрибут \"" *mark:ar-attr*
                     "\" (ожидалось: " txt ")"))
           (mark:note-warning)))
       obj))))

;; Пересечение интервалов (строгое; касание = можно в одну линию)
(defun mark:ar-overlap? (tlist lo hi / hit iv)
  (setq hit nil)
  (foreach iv tlist
    (if (and (< lo (cadr iv)) (> hi (car iv)))
      (setq hit t)))
  hit)

(defun mark:ar-track-get (tracks k)
  (if (and (numberp k) (>= k 0) (< k (length tracks)))
    (nth k tracks)
    nil))

(defun mark:ar-track-set (tracks k iv / out i res t0)
  (setq out tracks
        i   (length out))
  (while (<= i k)
    (setq out (append out (list nil))
          i   (1+ i)))
  (setq i   0
        res nil)
  (foreach t0 out
    (setq res (cons (if (= i k) iv t0) res)
          i   (1+ i)))
  (reverse res))

;; Упаковка рядовки — items: ((pos size text) …)
;;  1) Все кандидаты: size v (максимальный первым).
;;  2) First-fit: в первую линию, где нет наложения
;;     (интервал [pos?size/2 ; pos+size/2], касание = можно).
;;  3) Линия 0 > база; линия k?1 > k * *mark:ar-line* (150 мм).
;; Пример: 1550,590,530,460,320 > ряд0:1550, ряд1:590+530+320, ряд2:460
;; > ((k pos size text) …)
(defun mark:ar-pack (items / sorted tracks out
                        item pos size lo hi k ok tlist)
  (setq sorted (vl-sort (append items nil)
                 '(lambda (a b)
                    (> (float (cadr a)) (float (cadr b)))))
        tracks nil
        out    nil)
  (foreach item sorted
    (setq pos  (float (car item))
          size (float (cadr item))
          lo   (- pos (/ size 2.0))
          hi   (+ pos (/ size 2.0))
          k    0
          ok   nil)
    (while (null ok)
      (setq tlist (mark:ar-track-get tracks k))
      (if (mark:ar-overlap? tlist lo hi)
        (setq k (1+ k))
        (progn
          (setq tracks (mark:ar-track-set tracks k
                         (cons (list lo hi) tlist))
                out    (cons (list k pos size (caddr item)) out)
                ok     t)))))
  (reverse out))

(defun mark:ar-vla->ename (obj / e)
  (setq e (vl-catch-all-apply 'vlax-vla-object->ename (list obj)))
  (if (vl-catch-all-error-p e) nil e))

;; Группа AutoCAD по списку enames; nil при <1 объекта
(defun mark:ar-make-group (name enames / doc groups old grp arr i n)
  (if (and enames (> (length enames) 1))
    (progn
      (setq doc    (mark:ax-get (vlax-get-acad-object) "ActiveDocument")
            groups (if doc (mark:ax-get doc "Groups") nil))
      (if (null groups)
        (mark:out "[WARN] Groups недоступны.")
        (progn
          ;; удалить одноимённую группу
          (setq old (mark:ax-invoke groups "Item" name))
          (if old
            (vl-catch-all-apply 'vla-Delete (list old)))
          (setq grp (vl-catch-all-apply 'vla-Add (list groups name)))
          (cond
            ((vl-catch-all-error-p grp)
             (mark:out (strcat "[WARN] Группа \"" name "\": "
                               (vl-catch-all-error-message grp))))
            ((null grp)
             (mark:out (strcat "[WARN] Группа \"" name "\" не создана.")))
            (t
             (progn
               (setq n   (length enames)
                     arr (vlax-make-safearray vlax-vbObject
                            (cons 0 (1- n)))
                     i   0)
               (foreach e enames
                 (vlax-safearray-put-element arr i
                   (vlax-ename->vla-object e))
                 (setq i (1+ i)))
               (vl-catch-all-apply 'vlax-invoke-method
                 (list grp "AppendItems" arr))
               (mark:out
                 (strcat "[INFO] Группа \"" name "\": "
                         (itoa n) " объект(ов)")))))))))
  nil)

;;;--- Основной проход ---------------------------------------------------

(defun mark:ar-main (/ r recs doc space placed
                     y-base x-base cols rows
                     g letter number subs s0 k ref
                     x y w h obj keysW keysH
                     cnt-h cnt-v y0 x1
                     h-ents v-ents he ve
                     h-items v-items pl)
  (mark:cmd-line
    "МАРКАРЯД — рядовка из блоков «Ряд заполнений»: номера снизу, буквы справа.")
  (mark:reset-state)
  (mark:ar-banner)

  (if (null (mark:select))
    (princ)
    (progn
      (if (null *mark:fills*)
        (progn
          (mark:out "[ERROR] Заполнения не выбраны.")
          (mark:note-error))
        (progn
          (setq recs (vl-catch-all-apply 'mark:ar-collect (list *mark:fills*)))
          (cond
            ((vl-catch-all-error-p recs)
             (progn
               (mark:out "[ERROR] Сбой чтения заполнений:")
               (mark:out
                 (strcat "       "
                         (vl-catch-all-error-message recs)))
               (mark:note-error)))
            ((null recs)
             (mark:out "[ERROR] Нет пригодных заполнений (Марка/W/H)."))
            (t
             (progn
               (mark:out
                 (strcat "[INFO] Пригодных заполнений: "
                         (itoa (length recs))))
               (mark:out
                 (strcat "[INFO] Индексы рядовки: номер=ширина ("
                         (itoa (length *mark:widths*))
                         "), буква=высота ("
                         (itoa (length *mark:heights*))
                         ")."))

               ;; ключи dynamic-свойств ряда: сначала как у заполнения,
               ;; затем универсальные
               ;; свойство «Ширина»/«Высота» блока Ряд — в приоритете
               (setq keysW (cons "Ширина" (cons "Width" *mtab:w-keys*))
                     keysH (cons "Высота" (cons "Height" *mtab:h-keys*)))

               (setq doc    (mark:ax-get (vlax-get-acad-object)
                                         "ActiveDocument")
                     space  (if doc (mark:ax-get doc "ModelSpace") nil)
                     placed (mark:ar-placed-pts)
                     cnt-h  0
                     cnt-v  0
                     h-ents nil
                     v-ents nil)

               (if (null space)
                 (progn
                   (mark:out "[ERROR] ModelSpace недоступен.")
                   (mark:note-error)))

               (if space
                 (progn
                   (if (and doc (not *mark:batch-undo*))
                     (mark:ax-invoke-ok doc "StartUndoMark" nil))
                   ;; старые буквы/номера на тех же точках иначе пропускает ar-exists
                   (mark:ar-erase-old recs)
                   (setq placed (mark:ar-placed-pts))

                   ;; ===== ГОРИЗОНТАЛЬ (номера по ширине) =====
                   (setq y-base nil)
                   (foreach r recs
                     (setq y0 (nth 7 r))
                     (if (or (null y-base) (< y0 y-base))
                       (setq y-base y0)))
                   (setq y-base (- (float y-base) *mark:ar-offset*))
                   (mark:out
                     (strcat "[INFO] Горизонтальный ряд Y = "
                             (rtos y-base 2 1)))

                   ;; кандидаты: (cx W НОМЕР) — столбцы по cx, подгруппы W
                   (setq cols (mark:ar-group-idx recs 3 *mark:ar-tol*)
                         h-items nil)
                   (foreach g cols
                     (setq subs (mark:ar-subgroup g 9 *mark:ar-tol*))
                     (foreach s0 subs
                       ;; номер — от заполнения ЭТОЙ подгруппы (своя ширина),
                       ;; не от первого в колонке
                       (setq ref    (car s0)
                             number (nth 1 ref))
                       (setq h-items
                         (cons (list (nth 3 ref) (nth 9 ref) number)
                               h-items))))
                   ;; упаковка: max W > ряд 0; наложения > ряд 1+ (по убыв.)
                   (foreach pl (mark:ar-pack h-items)
                     (setq k     (nth 0 pl)
                           x     (nth 1 pl)
                           w     (nth 2 pl)
                           number (nth 3 pl)
                           y     (- y-base (* k *mark:ar-line*)))
                     (if (not (mark:ar-exists x y placed))
                       (progn
                         (setq obj (mark:ar-insert space x y number 270.0))
                         (if obj
                           (progn
                             (setq he    (mark:ar-vla->ename obj)
                                   h-ents (if he (cons he h-ents) h-ents))
                             ;; (W/2 + 25): осевая + половина на сторону
                             (if (not (mark:ar-put-dim obj keysW
                                              (+ (/ w 2.0) 25.0)))
                               (progn
                                 (mark:out
                                   (strcat "[WARN] Ширина не задана ("
                                           (rtos (+ (/ w 2.0) 25.0) 2 1)
                                           ")"))
                                 (mark:note-warning)))
                             (setq placed (cons (list x y) placed)
                                   cnt-h  (1+ cnt-h)))))))

                   ;; ===== ВЕРТИКАЛЬ (буквы по высоте), справа =====
                   (setq x-base nil)
                   (foreach r recs
                     (setq x1 (nth 6 r))
                     (if (or (null x-base) (> x1 x-base))
                       (setq x-base x1)))
                   (setq x-base (+ (float x-base) *mark:ar-offset*))
                   (mark:out
                     (strcat "[INFO] Вертикальный ряд X = "
                             (rtos x-base 2 1)))

                   ;; кандидаты: (cy H БУКВА) — строки по cy, подгруппы H
                   (setq rows   (mark:ar-group-idx recs 4 *mark:ar-tol*)
                         v-items nil)
                   (foreach g rows
                     (setq subs (mark:ar-subgroup g 10 *mark:ar-tol*))
                     (foreach s0 subs
                       ;; буква — от заполнения ЭТОЙ подгруппы (своя высота),
                       ;; не от первого в строке
                       (setq ref    (car s0)
                             letter (if (mark:strp (nth 0 ref))
                                      (strcase (nth 0 ref))
                                      ""))
                       (setq v-items
                         (cons (list (nth 4 ref) (nth 10 ref) letter)
                               v-items))))
                   ;; упаковка: max H > колонка 0; наложения > 1+ (по убыв.)
                   (foreach pl (mark:ar-pack v-items)
                     (setq k      (nth 0 pl)
                           y      (nth 1 pl)
                           h      (nth 2 pl)
                           letter (nth 3 pl)
                           x      (+ x-base (* k *mark:ar-line*)))
                     (if (not (mark:ar-exists x y placed))
                       (progn
                         (setq obj (mark:ar-insert space x y letter 0.0))
                         (if obj
                           (progn
                             (setq ve    (mark:ar-vla->ename obj)
                                   v-ents (if ve (cons ve v-ents) v-ents))
                             ;; в «Ширина»: (H/2 + 25)
                             (if (not (mark:ar-put-dim obj keysW
                                              (+ (/ h 2.0) 25.0)))
                               (progn
                                 (mark:out
                                   (strcat "[WARN] Ширина не задана ("
                                           (rtos (+ (/ h 2.0) 25.0) 2 1)
                                           ")"))
                                 (mark:note-warning)))
                             (setq placed (cons (list x y) placed)
                                   cnt-v  (1+ cnt-v)))))))

                   ;; группы: горизонталь и вертикаль раздельно
                   (mark:ar-make-group *mark:ar-group-h* h-ents)
                   (mark:ar-make-group *mark:ar-group-v* v-ents)

                   (if (and doc (not *mark:batch-undo*))
                     (mark:ax-invoke-ok doc "EndUndoMark" nil))

                   (mark:out
                     (strcat "[INFO] Горизонтальных (номера): "
                             (itoa cnt-h)))
                   (mark:out
                     (strcat "[INFO] Вертикальных (буквы): "
                             (itoa cnt-v)))
                   (mark:out
                     (strcat "[INFO] Вставлено: "
                             (itoa (+ cnt-h cnt-v)))))))))))))

  (mark:out (strcat "Ошибок: " (itoa *mark:errors*)))
  (mark:out (strcat "Предупреждений: " (itoa *mark:warnings*)))
  (if (= *mark:errors* 0)
    (mark:out "[INFO] Готово."))
  (princ))


;;;=====================================================================
;;;  МАРКАБЛОК / MARKFILL — вставка «Заполнение в витраж» по ячейкам
;;;  Режимы, в этом порядке:
;;;    Сетка-мультилинии, Точка-мультилинии, Сетка-динамика,
;;;    Точка-динамика, Полилинии
;;;  Вставка: точный левый нижний угол. W/H блока — до миллиметра.
;;;  Атрибуты обнуляются — пользователь заполняет сам.
;;;=====================================================================

;; *mark:fill-tol* *mark:fill-slope* *mark:fill-window* — в шапке
(setq *mark:mline-warns* 0)          ; счётчик warn MLINE (макс 3)

;;; ---- габарит сущности > (x0 y0 x1 y1) | nil --------------------------

(defun mark:cell-bb (e / ed typ obj r mn mx L0 L1
                       x0 y0 x1 y1 xs ys p)
  (setq ed (entget e))
  (if (null ed)
    nil
    (progn
      (setq typ (cdr (assoc 0 ed)))
      (cond
        ((and (= typ "LWPOLYLINE")
              (cdr (assoc 70 ed))
              (= 1 (logand 1 (cdr (assoc 70 ed)))))
         (progn
           (setq xs nil
                 ys nil)
           (foreach p ed
             (if (= (car p) 10)
               (progn
                 (setq xs (cons (float (cadr p)) xs)
                       ys (cons (float (caddr p)) ys)))))
           (if (and xs ys)
             (progn
               (setq x0 (car xs)
                     x1 (car xs)
                     y0 (car ys)
                     y1 (car ys))
               (foreach p xs
                 (if (< p x0) (setq x0 p))
                 (if (> p x1) (setq x1 p)))
               (foreach p ys
                 (if (< p y0) (setq y0 p))
                 (if (> p y1) (setq y1 p)))
               (list x0 y0 x1 y1))
             nil)))
        (t
         (progn
           (setq obj (mark:ax-catch-vla e))
           (if (null obj)
             nil
             (progn
               (setq r (vl-catch-all-apply 'vla-GetBoundingBox
                         (list obj 'mn 'mx)))
               (if (or (vl-catch-all-error-p r) (null r))
                 nil
                 (progn
                   (setq L0 (vl-catch-all-apply 'vlax-safearray->list (list mn))
                         L1 (vl-catch-all-apply 'vlax-safearray->list (list mx)))
                   (if (or (vl-catch-all-error-p L0)
                           (vl-catch-all-error-p L1)
                           (null L0) (null L1)
                           (< (length L0) 2) (< (length L1) 2))
                     nil
                     (progn
                       (setq x0 (float (car L0))
                             y0 (float (cadr L0))
                             x1 (float (car L1))
                             y1 (float (cadr L1)))
                       (list (min x0 x1) (min y0 y1)
                             (max x0 x1) (max y0 y1))))))))))))))

(defun mark:ax-catch-vla (e / obj)
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list e)))
  (if (or (vl-catch-all-error-p obj) (null obj)) nil obj))

;;; ---- атрибуты обнулить ------------------------------------------------

(defun mark:fill-clear-attrs-ename (e / ed cnt)
  (setq cnt 0)
  (if (and e (not (vl-catch-all-error-p e)))
    (progn
      (setq e (entnext e))
      (while e
        (setq ed (entget e))
        (if (and ed (= "ATTRIB" (cdr (assoc 0 ed))))
          (progn
            (if (assoc 1 ed)
              (entmod (subst (cons 1 "") (assoc 1 ed) ed))
              (entmod (append ed (list (cons 1 "")))))
            (setq cnt (1+ cnt))))
        (setq e (entnext e)))))
  cnt)

(defun mark:fill-clear-attrs (obj / e)
  (setq e (vl-catch-all-apply 'vlax-vla-object->ename (list obj)))
  (if (and (not (vl-catch-all-error-p e)) e)
    (mark:fill-clear-attrs-ename e)
    0))

;;; ---- вставка + W/H ----------------------------------------------------
(defun mark:fill-block-there ()
  (if (tblsearch "BLOCK" *mark:block-fill*)
    t
    (progn
      (if (null *mark:fill-miss*)
        (progn
          (setq *mark:fill-miss* t)
          (mark:out
            (strcat "[ERROR] В чертеже нет блока «" *mark:block-fill* "»."))))
      nil)))

(defun mark:fill-attdefs (bname / rec e ed out)
  (setq rec (tblsearch "BLOCK" bname)
        e   (if rec (cdr (assoc -2 rec)) nil)
        out nil)
  (while (and e (setq ed (entget e)) (/= "ENDBLK" (cdr (assoc 0 ed))))
    (if (= "ATTDEF" (cdr (assoc 0 ed)))
      (setq out (cons ed out)))
    (setq e (entnext e)))
  (reverse out))

(defun mark:fill-insert-ent (x y / e defs d tag pt h ang flags)
  (if (null (tblsearch "BLOCK" *mark:block-fill*))
    nil
    (progn
      (setq defs (mark:fill-attdefs *mark:block-fill*)
            e (vl-catch-all-apply 'entmakex
                (list (list '(0 . "INSERT")
                            (cons 2 *mark:block-fill*)
                            (list 10 (float x) (float y) 0.0)
                            '(41 . 1.0)
                            '(42 . 1.0)
                            '(43 . 1.0)
                            '(50 . 0.0)
                            (cons 66 (if defs 1 0))))))
      (if (or (vl-catch-all-error-p e) (null e))
        nil
        (progn
          (if defs
            (progn
              (foreach d defs
                (setq tag (cdr (assoc 2 d))
                      pt  (cdr (assoc 10 d))
                      h   (cdr (assoc 40 d))
                      ang (cdr (assoc 50 d))
                      flags (cdr (assoc 70 d)))
                (entmakex
                  (list '(0 . "ATTRIB")
                        (list 10
                              (+ (float x) (if (and pt (car pt)) (float (car pt)) 0.0))
                              (+ (float y) (if (and pt (cadr pt)) (float (cadr pt)) 0.0))
                              0.0)
                        (cons 2 (if tag tag "Марка"))
                        '(1 . "")
                        (cons 40 (if (numberp h) h 2.5))
                        (cons 70 (if (numberp flags) flags 0))
                        (cons 50 (if (numberp ang) ang 0.0)))))
              (entmakex '((0 . "SEQEND")))))
          (mark:out "[INFO] Вставка через entmakex.")
          (vl-catch-all-apply 'vlax-ename->vla-object (list e)))))))

(defun mark:fill-find-fill (/ ss i e nm best)
  (setq ss (vl-catch-all-apply 'ssget
             (list "X" (list '(0 . "INSERT"))))
        best nil)
  (if (and ss (not (vl-catch-all-error-p ss)))
    (progn
      (setq i (sslength ss))
      (while (and (> i 0) (null best))
        (setq i (1- i)
              e (ssname ss i)
              nm (mark:fill-eff-name e))
        (if (mark:name= nm *mark:block-fill*)
          (setq best e)))))
  best)

(defun mark:fill-insert-copy (x y / src obj copy)
  (setq src (mark:fill-find-fill)
        obj (if src (mark:ax-catch-vla src) nil)
        copy (if obj (vl-catch-all-apply 'vla-Copy (list obj)) nil))
  (if (or (null copy) (vl-catch-all-error-p copy))
    nil
    (progn
      (vl-catch-all-apply 'vla-Move
        (list copy
              (vl-catch-all-apply 'vla-get-InsertionPoint (list copy))
              (vlax-3d-point (list (float x) (float y) 0.0))))
      (mark:out "[INFO] Вставка копией блока, который уже есть в чертеже.")
      copy)))

(defun mark:fill-try-vla (space x y)
  (vl-catch-all-apply 'vla-InsertBlock
    (list space
          (vlax-3d-point (list (float x) (float y) 0.0))
          *mark:block-fill*
          1.0 1.0 1.0
          0.0)))

(defun mark:fill-end-undo (/ doc)
  ;; Группу режима «точка» не закрываем: её закроет конец захода.
  (if *mark:pt-undo*
    (setq *mark:fill-undo-off* t)
    (progn
      (setq doc (mark:ax-get (vlax-get-acad-object) "ActiveDocument"))
      (if doc (mark:ax-invoke-ok doc "EndUndoMark" nil))
      (setq *mark:fill-undo-off* t))))

(defun mark:fill-insert (space x y / obj err alt msg)
  (cond
    ((null (mark:fill-block-there)) nil)
    (t
     (progn
       (setq obj (mark:fill-try-vla space x y)
             err (if (vl-catch-all-error-p obj)
                   (vl-catch-all-error-message obj)
                   nil))
       (if (and err (null *mark:fill-undo-off*)
                (wcmatch (strcase err) "*FILER*,*ФАЙЛЕР*"))
         (progn
           (mark:out "[INFO] Ошибка файлера внутри UNDO — повторяю вставку без группы.")
           (mark:fill-end-undo)
           (setq obj (mark:fill-try-vla space x y)
                 err (if (vl-catch-all-error-p obj)
                       (vl-catch-all-error-message obj)
                       nil))))
       (if (or err (null obj))
         (setq alt (mark:fill-insert-copy x y)
               obj (if (and alt (not (vl-catch-all-error-p alt))) alt obj)))
       (if (or (null obj) (vl-catch-all-error-p obj))
         (setq alt (mark:fill-insert-ent x y)
               obj (if (and alt (not (vl-catch-all-error-p alt))) alt nil)))
       (if (and obj (not (vl-catch-all-error-p obj)))
         obj
         (progn
           (setq msg (if err err "блок не вставился"))
           (mark:out (strcat "[ERROR] INSERT: " msg))
           (mark:note-error)
           nil))))))

(defun mark:fill-set-dims (obj w h / keysW keysH okw okh)
  (setq keysW (cons *mark:prop-width* *mtab:w-keys*)
        keysH (cons *mark:prop-height* *mtab:h-keys*)
        okw (mark:ar-put-dim obj keysW (float w))
        okh (mark:ar-put-dim obj keysH (float h)))
  (if (not okw)
    (progn
      (mark:out (strcat "[WARN] Ширина в свету не задана ("
                        (rtos w 2 1) ")"))
      (mark:note-warning)))
  (if (not okh)
    (progn
      (mark:out (strcat "[WARN] Высота в свету не задана ("
                        (rtos h 2 1) ")"))
      (mark:note-warning)))
  (and okw okh))

;;; ---- список ячеек + дедуп --------------------------------------------

(defun mark:fill-add (cells pts bb / x0 y0 x1 y1 w h dup p)
  (if (null bb)
    (list cells pts)
    (progn
      (setq x0 (nth 0 bb)
            y0 (nth 1 bb)
            x1 (nth 2 bb)
            y1 (nth 3 bb)
            w  (- x1 x0)
            h  (- y1 y0)
            dup nil)
      (foreach p pts
        (if (and (null dup)
                 (<= (distance (list x0 y0 0.0)
                               (list (car p) (cadr p) 0.0))
                     *mark:ar-tol*))
          (setq dup t)))
      (if dup
        (progn
          (mark:out "[INFO] Ячейка уже в списке — пропуск.")
          (list cells pts))
        (progn
          (setq cells (cons bb cells)
                pts   (cons (list x0 y0) pts))
          (mark:out
            (strcat "  ячейка " (itoa (length cells))
                    ": " (rtos x0 2 4) "," (rtos y0 2 4)
                    "  W=" (rtos w 2 4)
                    "  H=" (rtos h 2 4)))
          (list cells pts))))))

;;; ---- вершины / точка в полигоне --------------------------------------

(defun mark:fill-poly-verts (e / ed verts p)
  (setq ed (entget e))
  (if (and ed
           (= "LWPOLYLINE" (cdr (assoc 0 ed)))
           (cdr (assoc 70 ed))
           (= 1 (logand 1 (cdr (assoc 70 ed)))))
    (progn
      (setq verts nil)
      (foreach p ed
        (if (= (car p) 10)
          (setq verts (cons (list (float (cadr p)) (float (caddr p)))
                            verts))))
      (reverse verts))
    nil))

(defun mark:fill-pt-in (x y verts / inside i j n xi yi xj yj)
  (setq inside nil
        i 0
        n (length verts))
  (while (< i n)
    (setq j  (rem (1+ i) n)
          xi (car (nth i verts))
          yi (cadr (nth i verts))
          xj (car (nth j verts))
          yj (cadr (nth j verts)))
    (if (and (/= (> yi y) (> yj y))
             (< x (+ xi (* (/ (- xj xi) (- yj yi)) (- y yi)))))
      (setq inside (not inside)))
    (setq i (1+ i)))
  inside)

(defun mark:fill-poly-area (verts / a i n x0 y0 x1 y1)
  (setq a 0.0
        i 0
        n (length verts))
  (while (< i n)
    (setq x0 (car (nth i verts))
          y0 (cadr (nth i verts))
          x1 (car (nth (rem (1+ i) n) verts))
          y1 (cadr (nth (rem (1+ i) n) verts)))
    (setq a (+ a (- (* x0 y1) (* x1 y0))))
    (setq i (1+ i)))
  (abs (* a 0.5)))

;;; ---- РЕЖИМ 1: полилинии -----------------------------------------------

(defun mark:fill-mode-poly (cells pts / sel e bb r)
  (mark:out "Тыкайте в границы ячеек (замкнутые полилинии и т.п.).")
  (mark:out "Enter — конец выбора.")
  (while
    (progn
      (setq sel
        (vl-catch-all-apply 'entsel
          (list "\nГраница ячейки <Enter — конец>: ")))
      (and (not (vl-catch-all-error-p sel)) sel))
    (setq e  (car sel)
          bb (mark:cell-bb e))
    (if (null bb)
      (mark:out "[WARN] Не удалось взять габарит — пропуск.")
      (progn
        (setq r   (mark:fill-add cells pts bb)
              cells (car r)
              pts   (cadr r)))))
  (list cells pts))

;;; ---- РЕЖИМ 2: точка внутри --------------------------------------------

(defun mark:fill-ss-near (pt win / ss filter)
  ;; 1) окно + фильтр типов
  (setq filter (list (cons 0 "LINE,LWPOLYLINE,POLYLINE,MLINE,ARC")))
  (setq ss
    (vl-catch-all-apply 'ssget
      (list "C"
            (list (- (car pt) win) (- (cadr pt) win))
            (list (+ (car pt) win) (+ (cadr pt) win))
            filter)))
  (if (or (vl-catch-all-error-p ss) (null ss))
    (progn
      ;; 2) окно без фильтра
      (setq ss
        (vl-catch-all-apply 'ssget
          (list "C"
                (list (- (car pt) win) (- (cadr pt) win))
                (list (+ (car pt) win) (+ (cadr pt) win)))))
      (if (or (vl-catch-all-error-p ss) (null ss))
        (progn
          (mark:out "[INFO] Окно пустое — берём все линии чертежа (X)…")
          ;; 3) все нужные типы в чертеже (как штриховка ищет границы)
          (setq ss
            (vl-catch-all-apply 'ssget (list "X" filter)))
          (if (or (vl-catch-all-error-p ss) (null ss))
            nil
            ss))
        ss))
    ss))

(defun mark:fill-segs-near (ss pt win / i e r g typ mid d dx dy lim
                               msegs osegs mnear onear
                               mcnt ocnt base near)
  ;; MLINE-предпочтение: если есть MLINE-сегменты — сетка только по ним
  (setq msegs nil
        osegs nil
        mnear nil
        onear nil
        i    (if ss (sslength ss) 0)
        lim  (* 1.4143 (float win)))
  (repeat i
    (setq i  (1- i)
          e  (ssname ss i)
          r  (vl-catch-all-apply 'mark:fill-extract-segs (list e)))
    (if (and (not (vl-catch-all-error-p r)) r)
      (progn
        (setq typ (cdr (assoc 0 (entget e))))
        (foreach g r
          (setq mid (list (/ (+ (nth 0 g) (nth 2 g)) 2.0)
                          (/ (+ (nth 1 g) (nth 3 g)) 2.0))
                dx  (- (car mid) (car pt))
                dy  (- (cadr mid) (cadr pt))
                d   (sqrt (+ (* dx dx) (* dy dy))))
          (if (and typ (= typ "MLINE"))
            (progn
              (setq msegs (cons g msegs))
              (if (<= d lim)
                (setq mnear (cons g mnear))))
            (progn
              (setq osegs (cons g osegs))
              (if (<= d lim)
                (setq onear (cons g onear)))))))))
  (setq msegs (reverse msegs)
        osegs (reverse osegs)
        mnear (reverse mnear)
        onear (reverse onear)
        mcnt  (length msegs)
        ocnt  (length osegs))
  (if (> mcnt 0)
    (progn
      (setq base msegs
            near mnear))
    (progn
      (setq base (append msegs osegs)
            near (append mnear onear))))
  (mark:out
    (strcat "[INFO] Отрезков MLINE: " (itoa mcnt)
            "  прочих: " (itoa ocnt)
            "  близко: " (itoa (length near))))
  (if (and near (>= (length near) 4))
    near
    base))

(defun mark:fill-cell-by-rays (segs pt / px py left right bottom top
                                     x0 y0 x1 y1 sx sy is-h is-v hw)
  (setq px (float (car pt))
        py (float (cadr pt))
        left   nil
        right  nil
        bottom nil
        top    nil)
  (foreach seg segs
    (setq x0 (min (float (nth 0 seg)) (float (nth 2 seg)))
          x1 (max (float (nth 0 seg)) (float (nth 2 seg)))
          y0 (min (float (nth 1 seg)) (float (nth 3 seg)))
          y1 (max (float (nth 1 seg)) (float (nth 3 seg)))
          hw (if (and (cddddr seg) (numberp (nth 4 seg)))
               (float (nth 4 seg))
               25.0))
    (setq is-h (<= (- y1 y0) 10.0)
          is-v (<= (- x1 x0) 10.0))
    ;; Горизонтальный отрезок над или под точкой, перекрывающий X точки
    (if (and is-h
             (<= (- x0 20.0) px)
             (>= (+ x1 20.0) px))
      (progn
        (setq sy (/ (+ y0 y1) 2.0))
        (if (<= sy py)
          ;; Нижняя мультилиния -> верхняя полка внутреннего контура: sy + hw
          (if (or (null bottom) (> (+ sy hw) bottom))
            (setq bottom (+ sy hw)))
          ;; Верхняя мультилиния -> нижняя полка внутреннего контура: sy - hw
          (if (or (null top) (< (- sy hw) top))
            (setq top (- sy hw))))))
    ;; Вертикальный отрезок слева или справа от точки, перекрывающий Y точки
    (if (and is-v
             (<= (- y0 20.0) py)
             (>= (+ y1 20.0) py))
      (progn
        (setq sx (/ (+ x0 x1) 2.0))
        (if (<= sx px)
          ;; Левая мультилиния -> правая полка внутреннего контура: sx + hw
          (if (or (null left) (> (+ sx hw) left))
            (setq left (+ sx hw)))
          ;; Правая мультилиния -> левая полка внутреннего контура: sx - hw
          (if (or (null right) (< (- sx hw) right))
            (setq right (- sx hw)))))))
  (if (and left right bottom top
           (> (- right left) 50.0)
           (> (- top bottom) 50.0))
    ;; Координаты не округляем: база вставки — точный левый нижний угол.
    (list left bottom right top)
    nil))


;;; ---- каркас из блока под точкой (динамический блок тоже) ------------
;; ssget не видит линии внутри INSERT. Сетка по выбранным MLINE/LINE не трогается.
;; Матрица (a b c d e f): x' = a*x + b*y + c, y' = d*x + e*y + f.

(defun mark:fill-mat-mul (p c / pa pb pc pd pe pf ca cb cc cd ce cf)
  (setq pa (nth 0 p) pb (nth 1 p) pc (nth 2 p)
        pd (nth 3 p) pe (nth 4 p) pf (nth 5 p)
        ca (nth 0 c) cb (nth 1 c) cc (nth 2 c)
        cd (nth 3 c) ce (nth 4 c) cf (nth 5 c))
  (list (+ (* pa ca) (* pb cd))
        (+ (* pa cb) (* pb ce))
        (+ (* pa cc) (* pb cf) pc)
        (+ (* pd ca) (* pe cd))
        (+ (* pd cb) (* pe ce))
        (+ (* pd cf) (* pe cf) pf)))

(defun mark:fill-mat-of (ed / p sx sy rot cs sn)
  (setq p   (cdr (assoc 10 ed))
        sx  (cdr (assoc 41 ed))
        sy  (cdr (assoc 42 ed))
        rot (cdr (assoc 50 ed)))
  (if (or (null sx) (not (numberp sx))) (setq sx 1.0))
  (if (or (null sy) (not (numberp sy))) (setq sy 1.0))
  (if (or (null rot) (not (numberp rot))) (setq rot 0.0))
  (if (null p) (setq p (list 0.0 0.0 0.0)))
  (setq cs (cos rot)
        sn (sin rot))
  (list (* sx cs) (* -1.0 sy sn) (float (car p))
        (* sx sn) (* sy cs)       (float (cadr p))))

(defun mark:fill-mat-pt (m pt / x y)
  (setq x (float (car pt))
        y (float (cadr pt)))
  (list (+ (* (nth 0 m) x) (* (nth 1 m) y) (nth 2 m))
        (+ (* (nth 3 m) x) (* (nth 4 m) y) (nth 5 m))))

(defun mark:fill-mat-scale (m / sx sy)
  (setq sx (sqrt (+ (* (nth 0 m) (nth 0 m)) (* (nth 3 m) (nth 3 m))))
        sy (sqrt (+ (* (nth 1 m) (nth 1 m)) (* (nth 4 m) (nth 4 m)))))
  (max sx sy 0.001))

(defun mark:fill-segs-xform (segs m / out s p0 p1 sc hw)
  (setq out nil
        sc  (mark:fill-mat-scale m))
  (foreach s segs
    (setq p0 (mark:fill-mat-pt m (list (nth 0 s) (nth 1 s)))
          p1 (mark:fill-mat-pt m (list (nth 2 s) (nth 3 s)))
          hw (if (and (> (length s) 4) (numberp (nth 4 s)))
               (* (float (nth 4 s)) sc)
               nil))
    (setq out
      (cons (if hw
              (list (car p0) (cadr p0) (car p1) (cadr p1) hw)
              (list (car p0) (cadr p0) (car p1) (cadr p1)))
            out)))
  (reverse out))

(defun mark:fill-arc-segs (ed / cen rad a0 a1 n i ang verts)
  (setq cen (cdr (assoc 10 ed))
        rad (cdr (assoc 40 ed))
        a0  (cdr (assoc 50 ed))
        a1  (cdr (assoc 51 ed)))
  (if (and cen rad a0 a1)
    (progn
      (if (< a1 a0)
        (setq a1 (+ a1 (* 2.0 pi))))
      (setq n 8
            i 0
            verts nil)
      (while (<= i n)
        (setq ang (+ a0 (* (/ (- a1 a0) (float n)) i))
              verts (cons (list (+ (float (car cen)) (* (float rad) (cos ang)))
                                (+ (float (cadr cen)) (* (float rad) (sin ang))))
                          verts)
              i (1+ i)))
      (mark:fill-verts-to-segs (reverse verts) nil))
    nil))

(defun mark:fill-poly-local (e / ed p rad verts closed)
  (setq ed (entget e)
        verts nil
        closed nil
        p (entnext e))
  (if (and ed (cdr (assoc 70 ed)) (= 1 (logand 1 (cdr (assoc 70 ed)))))
    (setq closed t))
  (while (and p (setq rad (entget p)) (/= "SEQEND" (cdr (assoc 0 rad))))
    (if (and (= "VERTEX" (cdr (assoc 0 rad))) (assoc 10 rad))
      (setq verts (cons (list (float (car (cdr (assoc 10 rad))))
                              (float (cadr (cdr (assoc 10 rad)))))
                        verts)))
    (setq p (entnext p)))
  (setq verts (reverse verts))
  (if (>= (length verts) 2)
    (mark:fill-verts-to-segs verts closed)
    nil))

;; Отрезки в системе блока. LINE/ARC читаем сами: extract-segs для LINE
;; берёт cadr после cdr и для сетки чертежа его не меняем.
(defun mark:fill-local-segs (e typ ed / p q)
  (cond
    ((= typ "LINE")
     (setq p (cdr (assoc 10 ed))
           q (cdr (assoc 11 ed)))
     (if (and p q)
       (list (list (float (car p)) (float (cadr p))
                   (float (car q)) (float (cadr q))))
       nil))
    ((= typ "ARC")
     (mark:fill-arc-segs ed))
    ((= typ "POLYLINE")
     (mark:fill-poly-local e))
    (t
     (mark:fill-extract-segs e))))

(defun mark:fill-skip-block? (nm)
  (and (mark:strp nm)
       (or (mark:name= nm *mark:block-fill*)
           (mark:name= nm *mark:block-glazing*)
           (mark:name= nm *mark:ar-block*))))

(defun mark:fill-eff-name (e / obj nm)
  (setq obj (mark:vla e)
        nm  (if obj (mark:ax-get obj "EffectiveName") nil))
  (if (mark:strp nm)
    nm
    (cdr (assoc 2 (entget e)))))

(defun mark:fill-def-segs (bname mat depth / rec e ed typ segs sub nm m2)
  (if (or (not (mark:strp bname)) (= bname "") (>= depth 6))
    nil
    (progn
      (setq rec (tblsearch "BLOCK" bname)
            e   (if rec (cdr (assoc -2 rec)) nil)
            segs nil)
      (while (and e (setq ed (entget e)) (/= "ENDBLK" (cdr (assoc 0 ed))))
        (setq typ (cdr (assoc 0 ed)))
        (cond
          ((= typ "INSERT")
           (setq nm  (cdr (assoc 2 ed))
                 m2  (mark:fill-mat-mul mat (mark:fill-mat-of ed))
                 sub (if (mark:fill-skip-block? nm)
                       nil
                       (mark:fill-def-segs nm m2 (1+ depth)))
                 segs (append sub segs)))
          ((member typ '("LINE" "ARC" "LWPOLYLINE" "POLYLINE" "MLINE"))
           (setq sub (mark:fill-local-segs e typ ed)
                 segs (append (mark:fill-segs-xform sub mat) segs))))
        (setq e (entnext e)))
      segs)))

(defun mark:fill-mat-inv (m / a b c d e f det)
  (setq a (nth 0 m) b (nth 1 m) c (nth 2 m)
        d (nth 3 m) e (nth 4 m) f (nth 5 m)
        det (- (* a e) (* b d)))
  (if (< (abs det) 1.0e-9)
    nil
    (list (/ e det) (/ (- b) det) (/ (- (* b f) (* e c)) det)
          (/ (- d) det) (/ a det) (/ (- (* d c) (* a f)) det))))

(defun mark:fill-pt-wcs (pt / p)
  (setq p (vl-catch-all-apply 'trans (list pt 1 0)))
  (if (or (vl-catch-all-error-p p) (null p))
    pt
    p))

(defun mark:fill-def-bb (bname / segs s x0 y0 x1 y1)
  (setq segs (mark:fill-def-segs bname (list 1.0 0.0 0.0 0.0 1.0 0.0) 0))
  (if (null segs)
    nil
    (progn
      (setq x0 (nth 0 (car segs))
            y0 (nth 1 (car segs))
            x1 x0
            y1 y0)
      (foreach s segs
        (setq x0 (min x0 (nth 0 s) (nth 2 s))
              y0 (min y0 (nth 1 s) (nth 3 s))
              x1 (max x1 (nth 0 s) (nth 2 s))
              y1 (max y1 (nth 1 s) (nth 3 s))))
      (list x0 y0 x1 y1))))

(defun mark:fill-def-bb-cached (bname / pair bb)
  (setq pair (assoc bname *mark:dyn-bb*))
  (if pair
    (cdr pair)
    (progn
      (setq bb (mark:fill-def-bb bname))
      (setq *mark:dyn-bb* (cons (cons bname bb) *mark:dyn-bb*))
      bb)))

(defun mark:fill-pt-in-ins (pt e / ed mat inv bb lp nm)
  (setq ed (entget e)
        nm (if ed (cdr (assoc 2 ed)) nil)
        mat (if ed (mark:fill-mat-of ed) nil)
        inv (if mat (mark:fill-mat-inv mat) nil)
        bb (if (mark:strp nm) (mark:fill-def-bb-cached nm) nil))
  (if (or (null inv) (null bb))
    nil
    (progn
      (setq lp (mark:fill-mat-pt inv pt))
      (and (>= (car lp) (- (nth 0 bb) 1.0))
           (<= (car lp) (+ (nth 2 bb) 1.0))
           (>= (cadr lp) (- (nth 1 bb) 1.0))
           (<= (cadr lp) (+ (nth 3 bb) 1.0))))))

(defun mark:fill-hits-best (hits / best)
  (setq best nil)
  (foreach h hits
    (if (or (null best) (< (car h) (car best)))
      (setq best h)))
  best)

(defun mark:fill-hits-drop (hits h / out dropped)
  (setq out nil
        dropped nil)
  (foreach x hits
    (if (and (null dropped)
             (eq (cadr x) (cadr h)))
      (setq dropped t)
      (setq out (cons x out))))
  (reverse out))

(defun mark:fill-hits-sort (hits / best out)
  (setq out nil)
  (while hits
    (setq best (mark:fill-hits-best hits)
          out  (cons best out)
          hits (mark:fill-hits-drop hits best)))
  (reverse out))

(defun mark:fill-hits-take (hits n / out)
  (setq out nil)
  (while (and hits (> n 0))
    (setq out (cons (car hits) out)
          hits (cdr hits)
          n (1- n)))
  (reverse out))

(defun mark:fill-hit-area (pt e / bb x y a)
  (setq x (float (car pt))
        y (float (cadr pt))
        bb (mark:cell-bb e)
        a nil)
  (if (and bb
           (>= x (nth 0 bb)) (<= x (nth 2 bb))
           (>= y (nth 1 bb)) (<= y (nth 3 bb))
           (> (- (nth 2 bb) (nth 0 bb)) 1.0)
           (> (- (nth 3 bb) (nth 1 bb)) 1.0))
    (setq a (* (- (nth 2 bb) (nth 0 bb))
               (- (nth 3 bb) (nth 1 bb)))))
  (if (and (null a) (mark:fill-pt-in-ins pt e))
    (progn
      (setq bb (mark:fill-def-bb-cached (cdr (assoc 2 (entget e)))))
      (if bb
        (setq a (max 1.0 (* (- (nth 2 bb) (nth 0 bb))
                            (- (nth 3 bb) (nth 1 bb))))))))
  a)

(defun mark:fill-ss-inserts (pt win / ss)
  (setq ss (vl-catch-all-apply 'ssget
             (list "C"
                   (list (- (float (car pt)) win) (- (float (cadr pt)) win))
                   (list (+ (float (car pt)) win) (+ (float (cadr pt)) win))
                   (list (cons 0 "INSERT")))))
  (if (or (vl-catch-all-error-p ss) (null ss))
    nil
    ss))

(defun mark:fill-ins-dist (pt e / ed p dx dy)
  (setq ed (entget e)
        p  (if ed (cdr (assoc 10 ed)) nil))
  (if (or (null p) (null (car p)) (null (cadr p)))
    1.0e99
    (progn
      (setq dx (- (float (car p)) (float (car pt)))
            dy (- (float (cadr p)) (float (cadr pt))))
      (sqrt (+ (* dx dx) (* dy dy))))))


(defun mark:fill-bb-dist (pt e / bb x y dx dy)
  (setq bb (mark:cell-bb e)
        x  (float (car pt))
        y  (float (cadr pt)))
  (if (and bb
           (> (abs (- (nth 2 bb) (nth 0 bb))) 1.0)
           (> (abs (- (nth 3 bb) (nth 1 bb))) 1.0))
    (progn
      (setq dx (max 0.0 (- (nth 0 bb) x) (- x (nth 2 bb)))
            dy (max 0.0 (- (nth 1 bb) y) (- y (nth 3 bb))))
      (sqrt (+ (* dx dx) (* dy dy))))
    (mark:fill-ins-dist pt e)))

(defun mark:fill-hit-report (hits / acc pair nm out first piece)
  (setq acc nil)
  (foreach hit hits
    (setq nm (mark:fill-eff-name (cadr hit))
          pair (assoc nm acc))
    (if pair
      (setq acc (subst (cons nm (1+ (cdr pair))) pair acc))
      (setq acc (cons (cons nm 1) acc))))
  (setq out nil
        first t)
  (foreach pair acc
    (setq piece (strcat (if (car pair) (car pair) "?") "=" (itoa (cdr pair))))
    (if first
      (setq first nil
            out piece)
      (setq out (strcat out " " piece))))
  (mark:out
    (strcat "[INFO] Блоков каркаса: " (itoa (length hits))
            (if out (strcat " (" out ")") ""))))

(defun mark:fill-near-inserts (pt win / ss i e nm d hits)
  (mark:out
    (strcat "[INFO] Ищу вставки в пределах " (rtos win 2 0) " от точки…"))
  (setq ss (vl-catch-all-apply 'ssget
             (list "X" (list (cons 0 "INSERT"))))
        hits nil)
  (if (and ss (not (vl-catch-all-error-p ss)))
    (progn
      (setq i (sslength ss))
      (repeat i
        (setq i (1- i)
              e (ssname ss i)
              nm (mark:fill-eff-name e))
        (if (not (mark:fill-skip-block? nm))
          (progn
            (setq d (mark:fill-bb-dist pt e))
            (if (<= d win)
              (setq hits (cons (list d e) hits))))))))
  (mark:fill-hits-take (mark:fill-hits-sort hits) 40))

(defun mark:fill-set-near (hits pt win / best d)
  (setq best nil)
  (foreach hit hits
    (setq d (mark:fill-bb-dist pt (cadr hit)))
    (if (or (null best) (< d best))
      (setq best d)))
  (and best (<= best win)))

(defun mark:fill-hits-segs (hits / segs part)
  (setq segs nil
        *mark:dyn-quiet* t)
  (foreach hit hits
    (setq part (mark:fill-segs-of-ins (cadr hit)))
    (if part
      (setq segs (append part segs))))
  (setq *mark:dyn-quiet* nil)
  segs)

(defun mark:fill-pick-frame (/ ss i e nm hits)
  (mark:out "[INFO] Выберите стойки и ригели рамкой. Enter — пропуск.")
  (setq ss (vl-catch-all-apply 'ssget
             (list (list (cons 0 "INSERT")))))
  (setq hits nil)
  (if (and ss (not (vl-catch-all-error-p ss)))
    (progn
      (setq i (sslength ss))
      (repeat i
        (setq i (1- i)
              e (ssname ss i)
              nm (mark:fill-eff-name e))
        (if (not (mark:fill-skip-block? nm))
          (setq hits (cons (list 0.0 e) hits))))))
  (if hits
    (mark:fill-hit-report hits))
  hits)

(defun mark:fill-insert-hits (ptu ptw / ss hits win)
  ;; Пустая ячейка не лежит внутри габарита стойки. Берём все вставки рядом.
  (setq win (if (and (numberp *mark:fill-window*) (> *mark:fill-window* 0.0))
              *mark:fill-window*
              5000.0)
        ss  (mark:fill-ss-inserts ptu win)
        hits nil)
  (if ss
    (setq hits (mark:fill-hits-take
                 (mark:fill-hits-sort (mark:fill-ss-hits ss ptw))
                 40)))
  (if (null hits)
    (setq hits (mark:fill-near-inserts ptw win)))
  (if (null hits)
    (setq hits (mark:fill-near-inserts ptw (* win 4.0))))
  (mark:fill-hit-report hits)
  hits)

(defun mark:fill-ss-hits (ss pt / i e nm hits)
  (setq hits nil
        i (if ss (sslength ss) 0))
  (repeat i
    (setq i (1- i)
          e (ssname ss i)
          nm (mark:fill-eff-name e))
    (if (not (mark:fill-skip-block? nm))
      (setq hits (cons (list (mark:fill-bb-dist pt e) e) hits))))
  hits)

(defun mark:fill-inserts-at (pt / hits)
  (setq hits (mark:fill-insert-hits pt pt))
  (if hits (cadr (car hits)) nil))

(defun mark:fill-block-segs-at (pt / e ed nm mat segs)
  (setq e (mark:fill-inserts-at pt))
  (if (null e)
    nil
    (progn
      (setq ed   (entget e)
            nm   (cdr (assoc 2 ed))
            mat  (mark:fill-mat-of ed)
            segs (mark:fill-def-segs nm mat 0))
      (mark:out
        (strcat "[INFO] Блок под точкой: «" (mark:fill-eff-name e)
                "», отрезков " (itoa (length segs))))
      segs)))

(defun mark:fill-seg-hw (s)
  (if (and (> (length s) 4) (numberp (nth 4 s)))
    (float (nth 4 s))
    25.0))

(defun mark:fill-box-closed (segs box / x0 y0 x1 y1 tol s xa ya xb yb hw sx sy
                                  okL okR okB okT)
  ;; Четыре стороны должны доходить до углов. Иначе это не ячейка.
  (setq x0 (nth 0 box)
        y0 (nth 1 box)
        x1 (nth 2 box)
        y1 (nth 3 box)
        tol *mark:fill-tol*
        okL nil
        okR nil
        okB nil
        okT nil)
  (foreach s segs
    (setq xa (min (float (nth 0 s)) (float (nth 2 s)))
          ya (min (float (nth 1 s)) (float (nth 3 s)))
          xb (max (float (nth 0 s)) (float (nth 2 s)))
          yb (max (float (nth 1 s)) (float (nth 3 s)))
          hw (mark:fill-seg-hw s)
          sx (/ (+ (float (nth 0 s)) (float (nth 2 s))) 2.0)
          sy (/ (+ (float (nth 1 s)) (float (nth 3 s))) 2.0))
    (if (and (<= (- xb xa) 10.0)
             (<= ya (+ y0 tol))
             (>= yb (- y1 tol)))
      (progn
        (if (<= (abs (- (+ sx hw) x0)) tol) (setq okL t))
        (if (<= (abs (- (- sx hw) x1)) tol) (setq okR t))))
    (if (and (<= (- yb ya) 10.0)
             (<= xa (+ x0 tol))
             (>= xb (- x1 tol)))
      (progn
        (if (<= (abs (- (+ sy hw) y0)) tol) (setq okB t))
        (if (<= (abs (- (- sy hw) y1)) tol) (setq okT t)))))
  (and okL okR okB okT))

(defun mark:fill-try-segs (segs pt cells pts / bb r)
  (setq bb (mark:fill-cell-by-rays segs pt))
  (cond
    ((and bb (mark:fill-box-closed segs bb))
     (mark:out "[INFO] Внутренний контур ячейки найден лучами.")
     (mark:fill-add cells pts bb))
    (bb
     (mark:out "[INFO] Лучи нашли незамкнутый контур — заполнение не ставлю.")
     nil)
    (t
     (setq r  (mark:fill-segs->cells segs)
           bb (mark:fill-smallest-cell (car r) pt))
     (mark:out
       (strcat "[INFO] Сетка блока: осей X " (itoa (cadr r))
               " Y " (itoa (caddr r))
               "  ячеек " (itoa (length (car r)))))
     (cond
       ((and bb (mark:fill-box-closed segs bb))
        (mark:fill-add cells pts bb))
       (bb
        (mark:out "[INFO] Ячейка сетки не замкнута — пропуск.")
        nil)
       (t nil)))))

(defun mark:fill-mode-point (cells pts / pt ss i e verts bb
                                 best best-area a r segs win)
  (setq pt (getpoint "\nТочка-мультилинии внутри ячейки <Enter — конец>: "))
  (if (null pt)
    (progn
      (mark:out "[INFO] Конец режима «Точка-мультилинии».")
      (list nil nil 'cancel))
    (progn
      (setq ss (vl-catch-all-apply 'ssget
                 (list "X" (list (cons 0 "LWPOLYLINE")))))
      (setq best nil
            best-area nil)
      (if (and ss (not (vl-catch-all-error-p ss)))
        (progn
          (setq i (sslength ss))
          (repeat i
            (setq i    (1- i)
                  e    (ssname ss i)
                  verts (mark:fill-poly-verts e))
            (if (and verts
                     (mark:fill-pt-in (car pt) (cadr pt) verts))
              (progn
                (setq a (mark:fill-poly-area verts))
                (if (or (null best-area) (< a best-area))
                  (progn
                    (setq best-area a
                          best e))))))))
      (if best
        (progn
          (setq bb (mark:cell-bb best))
          (if bb
            (progn
              (setq r    (mark:fill-add cells pts bb)
                    cells (car r)
                    pts   (cadr r))
              (list cells pts))
            (progn
              (mark:out "[WARN] Габарит ячейки не взят.")
              (list cells pts))))
        (progn
          (mark:out "[INFO] Замкнутой полилинии нет — ищем по сетке линий…")
          (setq win (if (and (numberp *mark:fill-window*)
                             (> *mark:fill-window* 0.0))
                      *mark:fill-window*
                      5000.0))
          (setq ss (mark:fill-ss-near pt win))
          (if (null ss)
            (progn
              (mark:out "[INFO] Линий не найдено (окно и X-выбор пусты).")
              (list cells pts))
            (progn
              (mark:fill-type-stat ss)
              (setq segs (mark:fill-segs-near ss pt win))
              (if (null segs)
                (progn
                  (mark:out "[WARN] Отрезки не извлечены — см. типы выше.")
                  (list cells pts))
                (progn
                  ;; Сначала ищем непосредственный локальный внутренний контур лучами вокруг точки
                  (setq bb (mark:fill-cell-by-rays segs pt))
                  (if bb
                    (progn
                      (mark:out "[INFO] Внутренний контур ячейки найден лучами.")
                      (setq r    (mark:fill-add cells pts bb)
                            cells (car r)
                            pts   (cadr r))
                      (list cells pts))
                    (progn
                      ;; Иначе расчет по общей сетке
                      (setq r  (mark:fill-segs->cells segs)
                            bb (mark:fill-smallest-cell (car r) pt))
                      (mark:out
                        (strcat "[INFO] Сетка: осей X " (itoa (cadr r))
                                " Y " (itoa (caddr r))
                                "  ячеек " (itoa (length (car r)))))
                      (if (null bb)
                        (progn
                          (mark:out "[INFO] Ячейка под точкой не найдена.")
                          (if (< (cadr r) 2)
                            (mark:out
                              "[INFO] Мало осей — увеличьте *mark:fill-window* или *mark:fill-tol*."))
                          (list cells pts))
                        (progn
                          (setq r    (mark:fill-add cells pts bb)
                                cells (car r)
                                pts   (cadr r))
                          (list cells pts))))))))))))))

(defun mark:fill-segs->cells (segs / xs ys sx sy x0 x1 y0 y1
                                  ix iy nxs nys out ax tmp v
                                  mx col-ys col-sy seg-x0 seg-x1 seg-y0
                                  has-v0 has-v1 seg-y1)
  (setq xs nil
        ys nil)
  (foreach seg segs
    (setq ax (mark:fill-axis seg))
    (cond
      ((eq ax 'v)
       (setq xs (cons (/ (+ (nth 0 seg) (nth 2 seg)) 2.0) xs)))
      ((eq ax 'h)
       (setq ys (cons (/ (+ (nth 1 seg) (nth 3 seg)) 2.0) ys)))))
  (setq sx (mark:fill-axes-avg xs)
        sy (mark:fill-axes-avg ys))
  ;; Фильтр ложных нулей
  (if (and sy (> (length sy) 1) (< (car sy) 50.0) (> (cadr sy) 1000.0))
    (setq sy (cdr sy)))
  (if (and sx (> (length sx) 1) (< (car sx) 50.0) (> (cadr sx) 1000.0))
    (setq sx (cdr sx)))
  ;; Оси не округляем: угол вставки сетки — точная координата.
  (setq nxs (length sx)
        nys (length sy)
        out nil)
  ;; Проходим по каждому вертикальному пролету [x0 .. x1]
  (if (and (>= nxs 2) (>= nys 2))
    (progn
      (setq ix 0)
      (while (< ix (1- nxs))
        (setq x0 (nth ix sx)
              x1 (nth (1+ ix) sx)
              mx (/ (+ x0 x1) 2.0)
              col-ys nil)
        ;; Отбираем только те горизонтальные ригели, которые физически перекрывают центр пролета mx
        (foreach seg segs
          (if (eq (mark:fill-axis seg) 'h)
            (progn
              (setq seg-x0 (min (float (nth 0 seg)) (float (nth 2 seg)))
                    seg-x1 (max (float (nth 0 seg)) (float (nth 2 seg)))
                    seg-y0 (/ (+ (nth 1 seg) (nth 3 seg)) 2.0))
              (if (and (<= (- seg-x0 *mark:fill-tol*) mx)
                       (>= (+ seg-x1 *mark:fill-tol*) mx))
                (setq col-ys (cons seg-y0 col-ys))))))
        (setq col-sy (mark:fill-axes-avg col-ys))
        (if (and col-sy (> (length col-sy) 1) (< (car col-sy) 50.0) (> (cadr col-sy) 1000.0))
          (setq col-sy (cdr col-sy)))
        ;; ВАЖНО: если в этом пролете нет минимум 2 собственных ригелей — это пустой межвитражный разрыв!
        ;; Никакого фолбэка на чужие глобальные ригели!
        (if (and col-sy (>= (length col-sy) 2))
          (progn
            (setq iy 0)
            (while (< iy (1- (length col-sy)))
              (setq y0 (nth iy col-sy)
                    y1 (nth (1+ iy) col-sy))
              ;; Проверяем, что стойки x0 и x1 физически соприкасаются с ячейкой [y0 .. y1]
              (setq has-v0 nil
                    has-v1 nil)
              (foreach seg segs
                (if (eq (mark:fill-axis seg) 'v)
                  (progn
                    (setq seg-y0 (min (float (nth 1 seg)) (float (nth 3 seg)))
                          seg-y1 (max (float (nth 1 seg)) (float (nth 3 seg)))
                          seg-x0 (/ (+ (nth 0 seg) (nth 2 seg)) 2.0))
                    (if (and (<= (- seg-y0 *mark:fill-tol*) y1)
                             (>= (+ seg-y1 *mark:fill-tol*) y0))
                      (progn
                        (if (<= (abs (- seg-x0 x0)) *mark:fill-tol*) (setq has-v0 t))
                        (if (<= (abs (- seg-x0 x1)) *mark:fill-tol*) (setq has-v1 t)))))))
              ;; Создаем ячейку с отступом полуширины профиля 25 мм
              (if (and has-v0 has-v1
                       (> (- x1 x0) 70.0)
                       (> (- y1 y0) 70.0))
                (setq out (cons (list (+ x0 25.0)
                                      (+ y0 25.0)
                                      (- x1 25.0)
                                      (- y1 25.0))
                                out)))
              (setq iy (1+ iy)))))
        (setq ix (1+ ix)))))
  (list (mark:fill-merge-t-cells out segs) nxs nys (length segs)))
(defun mark:fill-merge-t-cells (cells segs / changed out a b x0a y0a x1a y1a
                                           x0b y0b x1b y1b gap mid-x my has-col rest cand merged)
  (setq out cells
        changed t)
  (while changed
    (setq changed nil
          cand    out
          out     nil)
    (while cand
      (setq a    (car cand)
            cand (cdr cand)
            merged nil)
      (setq x0a (nth 0 a)
            y0a (nth 1 a)
            x1a (nth 2 a)
            y1a (nth 3 a))
      (setq rest nil)
      (foreach b cand
        ;; Проверяем, лежат ли ячейки на одном высотном уровне
        (if (and (null merged)
                 (<= (abs (- y0a (nth 1 b))) 5.0)
                 (<= (abs (- y1a (nth 3 b))) 5.0))
          (progn
            (setq x0b (nth 0 b)
                  x1b (nth 2 b))
            ;; Зазор между внутренними контурами ячеек (ширина профиля стойки ~50 мм)
            (setq gap (min (abs (- x0b x1a)) (abs (- x0a x1b))))
            (if (<= gap (+ 50.0 (* 2.0 *mark:fill-tol*)))
              (progn
                (setq mid-x (if (<= x1a x0b)
                              (/ (+ x1a x0b) 2.0)
                              (/ (+ x1b x0a) 2.0))
                      my    (/ (+ y0a y1a) 2.0)
                      has-col nil)
                ;; Проверяем, есть ли реальная стойка между этими двумя ячейками на высоте my
                (foreach seg segs
                  (if (eq (mark:fill-axis seg) 'v)
                    (progn
                      (if (and (<= (min (float (nth 1 seg)) (float (nth 3 seg))) (- my 15.0))
                               (>= (max (float (nth 1 seg)) (float (nth 3 seg))) (+ my 15.0))
                               (<= (abs (- (/ (+ (nth 0 seg) (nth 2 seg)) 2.0) mid-x)) (+ *mark:fill-tol* 10.0)))
                        (setq has-col t)))))
                (if (not has-col)
                  (progn
                    ;; Стойки между ячейками нет (Т-стык снизу!) — объединяем в одно широкое заполнение
                    (setq a (list (min x0a x0b) y0a (max x1a x1b) y1a)
                          merged  t
                          changed t))
                  (setq rest (cons b rest))))
              (setq rest (cons b rest))))
          (setq rest (cons b rest))))
      (setq cand rest
            out  (cons a out))))
  (reverse out))

(defun mark:fill-cell-has (bb pt)
  (and pt
       (>= (float (car pt)) (nth 0 bb))
       (<= (float (car pt)) (nth 2 bb))
       (>= (float (cadr pt)) (nth 1 bb))
       (<= (float (cadr pt)) (nth 3 bb))))

(defun mark:fill-smallest-cell (cells pt / best best-area a)
  (setq best nil
        best-area nil)
  (foreach bb cells
    (if (mark:fill-cell-has bb pt)
      (progn
        (setq a (* (- (nth 2 bb) (nth 0 bb))
                   (- (nth 3 bb) (nth 1 bb))))
        (if (or (null best-area) (< a best-area))
          (progn
            (setq best-area a
                  best bb))))))
  best)

;;; ---- РЕЖИМ 3: массив мультилиний > сетка ------------------------------

;; отрезки из LINE / LWPOLYLINE > ((x0 y0 x1 y1) ...)
(defun mark:fill-e-bb (e / obj r a b x0 y0 x1 y1)
  ;; AABB через GetBoundingBox с отсечением аномалий (0,0)
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list e)))
  (if (or (vl-catch-all-error-p obj) (null obj))
    nil
    (progn
      (setq a nil
            b nil)
      (setq r (vl-catch-all-apply 'vlax-invoke-method
                 (list obj "GetBoundingBox" 'a 'b)))
      (if (or (vl-catch-all-error-p r) (null a) (null b))
        nil
        (progn
          (setq a (vl-catch-all-apply 'vlax-safearray->list (list a))
                b (vl-catch-all-apply 'vlax-safearray->list (list b)))
          (if (or (vl-catch-all-error-p a) (vl-catch-all-error-p b)
                  (null a) (null b))
            nil
            (progn
              (setq x0 (float (car a))
                    y0 (float (cadr a))
                    x1 (float (car b))
                    y1 (float (cadr b)))
              ;; Отсекаем артефакты: если y0 или x0 схлопнулись в 0.0 при координатах > 1000
              (if (and (< y0 10.0) (> y1 1000.0))
                (setq y0 y1))
              (if (and (< x0 10.0) (> x1 1000.0))
                (setq x0 x1))
              (list x0 y0 x1 y1))))))))

;;; ---- разбор вершин > отрезки -----------------------------------------

(defun mark:fill-verts-to-segs (verts closed / segs n i p0 p1)
  (setq segs nil
        n    (length verts)
        i    0)
  (while (< i n)
    (setq p0 (nth i verts)
          p1 (nth (rem (1+ i) n) verts))
    (if (or (< i (1- n)) closed)
      (setq segs (cons (list (float (car p0)) (float (cadr p0))
                             (float (car p1)) (float (cadr p1)))
                       segs)))
    (setq i (1+ i)))
  (reverse segs))

(defun mark:round1 (v)
  (atof (rtos (float v) 2 0)))

(defun mark:mline-warn (msg)
  (if (and (numberp *mark:mline-warns*) (< *mark:mline-warns* 3))
    (progn
      (mark:out (strcat "[WARN] MLINE: " msg))
      (setq *mark:mline-warns* (1+ *mark:mline-warns*))
      (if (= *mark:mline-warns* 3)
        (mark:out
          "[WARN] MLINE: далее предупреждения о вершинах не выводятся."))))
  nil)

(defun mark:fill-extract-segs (e / ed typ verts segs i n ang p rad
                               bb x0 y0 x1 y1 w h yc xc tol closed)
  (setq ed   (entget e)
        segs nil)
  (if (null ed)
    nil
    (progn
      (setq typ (cdr (assoc 0 ed)))
      (cond
        ((= typ "LINE")
         (setq p   (cdr (assoc 10 ed))
               rad (cdr (assoc 11 ed)))
         (if (and p rad)
           (setq segs
             (list (list (float (cadr p))   (float (caddr p))
                         (float (cadr rad)) (float (caddr rad)))))))

        ((= typ "ARC")
         (setq p   (cdr (assoc 10 ed))
               rad (cdr (assoc 40 ed)))
         (if (and p rad (cdr (assoc 50 ed)) (cdr (assoc 51 ed)))
           (progn
             (setq n     16
                   i     0
                   verts nil)
             (while (<= i n)
               (setq ang   (+ (cdr (assoc 50 ed))
                              (* (/ (- (cdr (assoc 51 ed))
                                       (cdr (assoc 50 ed)))
                                    (float n))
                                 i))
                     ang   (* ang (/ pi 180.0))
                     verts (cons (list
                                   (+ (float (cadr p))
                                      (* (float rad) (cos ang)))
                                   (+ (float (caddr p))
                                      (* (float rad) (sin ang))))
                                 verts)
                     i     (1+ i)))
             (setq segs (mark:fill-verts-to-segs (reverse verts) nil)))))

        ((or (= typ "LWPOLYLINE") (= typ "POLYLINE"))
         (setq verts nil
               closed nil)
         (if (= typ "LWPOLYLINE")
           (progn
             (if (and (cdr (assoc 70 ed))
                      (= 1 (logand 1 (cdr (assoc 70 ed)))))
               (setq closed t))
             (foreach p ed
               (if (= (car p) 10)
                 (setq verts (cons (list (float (cadr p))
                                         (float (caddr p)))
                                   verts))))
             (setq verts (reverse verts)))
           (progn
             (if (and (cdr (assoc 70 ed))
                      (= 1 (logand 1 (cdr (assoc 70 ed)))))
               (setq closed t))
             (setq p (entnext e))
             (while p
               (setq rad (entget p))
               (if (or (null rad) (= "SEQEND" (cdr (assoc 0 rad))))
                 (setq p nil)
                 (progn
                   (if (= "VERTEX" (cdr (assoc 0 rad)))
                     (setq verts (cons (list (float (cadr (assoc 10 rad)))
                                             (float (cadr (assoc 11 rad))))
                                       verts)))
                   (setq p (entnext p)))))
             (setq verts (reverse verts))))
         (if (>= (length verts) 2)
           (setq segs (mark:fill-verts-to-segs verts closed))))

        ((= typ "MLINE")
         (setq verts nil
               tol   *mark:fill-tol*)
         (foreach p ed
           (if (= (car p) 10)
             (setq verts (cons (list (float (cadr p))
                                     (float (caddr p)))
                               verts))))
         (setq verts (reverse verts))
         (if (< (length verts) 2)
           (progn
             (setq verts nil)
             (foreach p ed
               (if (= (car p) 11)
                 (setq verts (cons (list (float (cadr p))
                                         (float (caddr p)))
                                   verts))))
             (setq verts (reverse verts))))
         (if (>= (length verts) 2)
           (setq segs (mark:fill-verts-to-segs verts nil))
           (progn
             (setq bb (mark:fill-e-bb e))
             (if (null bb)
               (mark:mline-warn "нет вершин и нет габарита — пропуск.")
               (progn
                 (setq x0 (nth 0 bb)
                       y0 (nth 1 bb)
                       x1 (nth 2 bb)
                       y1 (nth 3 bb)
                       w  (- x1 x0)
                       h  (- y1 y0))
                 (cond
                   ;; Горизонтальная мультилиния (осевая по yc, полуширина h/2)
                   ((and (>= w 10.0) (<= h (+ (* 3.0 tol) 10.0)))
                    (setq yc (/ (+ y0 y1) 2.0))
                    (setq segs (list (list x0 yc x1 yc (/ h 2.0)))))
                   ;; Вертикальная мультилиния (осевая по xc, полуширина w/2)
                   ((and (>= h 10.0) (<= w (+ (* 3.0 tol) 10.0)))
                    (setq xc (/ (+ x0 x1) 2.0))
                    (setq segs (list (list xc y0 xc y1 (/ w 2.0)))))
                   ;; Крупный габарит
                   ((and (> w 10.0) (> h 10.0))
                    (setq yc (/ (+ y0 y1) 2.0))
                    (setq segs (list (list x0 yc x1 yc 25.0))))
                   (t
                    (mark:mline-warn
                      (strcat "габарит слишком мал ("
                              (rtos w 2 1) "x" (rtos h 2 1) ").")))))))))
        (t nil))
      segs)))

(defun mark:fill-type-stat (ss / i e ed typ acc pair out first piece)
  (setq acc nil
        i   (if ss (sslength ss) 0))
  (repeat i
    (setq i   (1- i)
          e   (ssname ss i)
          ed  (entget e)
          typ (if (and ed (mark:strp (cdr (assoc 0 ed)))) (cdr (assoc 0 ed)) "?")
          pair (assoc typ acc))
    (if pair
      (setq acc (subst (cons typ (1+ (cdr pair))) pair acc))
      (setq acc (cons (cons typ 1) acc))))
  (setq out   nil
        first t)
  (foreach pair (reverse acc)
    (setq piece (strcat (car pair) "=" (itoa (cdr pair))))
    (if first
      (progn
        (setq first nil
              out   piece))
      (setq out (strcat out " " piece))))
  (mark:out (strcat "[INFO] Типы: " (if out out "(пусто)")))
  nil)

(defun mark:fill-axis (seg / dx dy adx ady ln)
  (setq dx  (- (nth 2 seg) (nth 0 seg))
        dy  (- (nth 3 seg) (nth 1 seg))
        adx (abs dx)
        ady (abs dy)
        ln  (sqrt (+ (* dx dx) (* dy dy))))
  (if (< ln 1e-6)
    nil
    (cond
      ((<= ady (* *mark:fill-slope* ln)) 'h)
      ((<= adx (* *mark:fill-slope* ln)) 'v)
      ((>= adx ady) 'h)
      (t 'v))))

(defun mark:fill-axes-avg (vals / sorted out cluster sum n v)
  (if (null vals)
    nil
    (progn
      (setq sorted  (vl-sort vals '<)
            out     nil
            cluster nil
            sum     0.0
            n       0)
      (foreach v sorted
        (if (and cluster (> (- v (car cluster)) *mark:fill-tol*))
          (progn
            (setq out     (cons (/ sum (float n)) out)
                  cluster (list v)
                  sum     v
                  n       1))
          (progn
            (if (null cluster)
              (setq cluster (list v))
              (setq cluster (cons v cluster)))
            (setq sum (+ sum v)
                  n   (1+ n)))))
      (if (and cluster (> n 0))
        (setq out (cons (/ sum (float n)) out)))
      (reverse out))))



(defun mark:fill-cluster-items (items / sorted out cluster v)
  (if (null items)
    nil
    (progn
      (setq sorted (vl-sort (append items nil)
                     '(lambda (a b) (< (car a) (car b))))
            out nil
            cluster nil)
      (foreach v sorted
        (if (and cluster (> (- (car v) (car (car cluster))) *mark:fill-tol*))
          (setq out (cons cluster out)
                cluster (list v))
          (setq cluster (cons v cluster))))
      (if cluster (setq out (cons cluster out)))
      (reverse out))))

(defun mark:fill-inner-edge (items sign / it e best)
  (setq best nil)
  (foreach it items
    (setq e (+ (nth 0 it) (* sign (nth 3 it))))
    (if (or (null best)
            (and (> sign 0.0) (> e best))
            (and (< sign 0.0) (< e best)))
      (setq best e)))
  best)

(defun mark:fill-item-spans (items / it out)
  (setq out nil)
  (foreach it items
    (setq out (cons (list (nth 1 it) (nth 2 it)) out)))
  out)

(defun mark:fill-covers (spans lo hi gap / s merged last a b p0 p1 hit)
  (setq merged nil
        hit nil)
  (if spans
    (foreach s (vl-sort (append spans nil)
                 '(lambda (a b) (< (car a) (car b))))
      (setq p0 (car s)
            p1 (cadr s))
      (if (null merged)
        (setq merged (list (list p0 p1)))
        (progn
          (setq last (car merged)
                a (car last)
                b (cadr last))
          (if (<= p0 (+ b gap))
            (setq merged (cons (list a (if (> p1 b) p1 b)) (cdr merged)))
            (setq merged (cons (list p0 p1) merged)))))))
  (foreach s merged
    (if (and (<= (car s) (+ lo gap))
             (>= (cadr s) (- hi gap)))
      (setq hit t)))
  hit)

(defun mark:fill-owners (items / it o out)
  (setq out nil)
  (foreach it items
    (setq o (nth 4 it))
    (if (not (member o out))
      (setq out (cons o out))))
  out)

(defun mark:fill-owners-differ (a b)
  (and a b (not (equal a b))))

(defun mark:fill-overlap-owners (items lo hi gap / it o out)
  (setq out nil)
  (foreach it items
    (if (and (<= (nth 1 it) (+ hi gap))
             (>= (nth 2 it) (- lo gap)))
      (progn
        (setq o (nth 4 it))
        (if (not (member o out))
          (setq out (cons o out))))))
  out)

(defun mark:fill-v-split (cols iL iR lo hi gap / i hit)
  (setq i (1+ iL)
        hit nil)
  (while (and (< i iR) (null hit))
    (if (mark:fill-covers (mark:fill-item-spans (nth i cols)) lo hi gap)
      (setq hit t))
    (setq i (1+ i)))
  hit)

(defun mark:fill-span-items (items lo hi gap / it out)
  (setq out nil)
  (foreach it items
    (if (mark:fill-covers (list (list (nth 1 it) (nth 2 it))) lo hi gap)
      (setq out (cons it out))))
  out)

(defun mark:fill-owned-axes (hits / i n vs hs part s x0 y0 x1 y1 dx dy ln hw nm ed)
  (setq i 0
        n (length hits)
        vs nil
        hs nil
        *mark:line-vis* 0
        *mark:expl-n* 0
        *mark:def-n* 0
        *mark:copy-left* 0
        *mark:dyn-quiet* t
        *mark:no-expl-undo* t)
  (foreach hit hits
    (setq i (1+ i)
          ed (entget (cadr hit))
          nm (if ed (cdr (assoc 2 ed)) nil)
          part (mark:fill-segs-of-ins (cadr hit)))
    (if (and part (not (mark:fill-mline-p nm)))
      (setq *mark:line-vis* (1+ *mark:line-vis*)))
    (if (and (> n 100) (= (rem i 250) 0))
      (mark:out
        (strcat "[INFO] Читаю блоки: " (itoa i) " из " (itoa n))))
    (foreach s part
      (setq x0 (min (float (nth 0 s)) (float (nth 2 s)))
            y0 (min (float (nth 1 s)) (float (nth 3 s)))
            x1 (max (float (nth 0 s)) (float (nth 2 s)))
            y1 (max (float (nth 1 s)) (float (nth 3 s)))
            dx (- x1 x0)
            dy (- y1 y0)
            ln (sqrt (+ (* dx dx) (* dy dy))))
      (if (>= ln 30.0)
        (progn
          (setq hw (if (and (> (length s) 4) (numberp (nth 4 s)))
                     (float (nth 4 s))
                     25.0))
          (cond
            ((<= dy (* *mark:fill-slope* ln))
             (setq hs (cons (list (/ (+ (float (nth 1 s)) (float (nth 3 s))) 2.0)
                                  x0 x1 hw i)
                            hs)))
            ((<= dx (* *mark:fill-slope* ln))
             (setq vs (cons (list (/ (+ (float (nth 0 s)) (float (nth 2 s))) 2.0)
                                  y0 y1 hw i)
                            vs))))))))
  (setq *mark:dyn-quiet* nil
        *mark:no-expl-undo* nil)
  (if (or (and (numberp *mark:expl-n*) (> *mark:expl-n* 0))
          (and (numberp *mark:def-n*) (> *mark:def-n* 0)))
    (mark:out
      (strcat "[INFO] Геометрия: из определения "
              (itoa (if (numberp *mark:def-n*) *mark:def-n* 0))
              ", разборка "
              (itoa (if (numberp *mark:expl-n*) *mark:expl-n* 0)))))
  (if (and (numberp *mark:copy-left*) (> *mark:copy-left* 0))
    (mark:out
      (strcat "[WARN] Не удалены копии блоков: " (itoa *mark:copy-left*))))
  (mark:out
    (strcat "[INFO] Отрезков каркаса: " (itoa (+ (length vs) (length hs)))
            "  вертикальных " (itoa (length vs))
            "  горизонтальных " (itoa (length hs))
            (if (and (numberp *mark:line-vis*) (> *mark:line-vis* 0))
              (strcat ". Видимых блоков из линий: " (itoa *mark:line-vis*)
                      ", отступ 0")
              "")))
  (list vs hs))

(defun mark:fill-closed-cells (hits / axes vs hs cols rows iL iR nL nR
                                     Lcol Rcol xL xR w spanning iB nB
                                     bot top yB yT h gap boxes
                                     oL oR n-own n-open)
  ;; Ячейка только если четыре стороны доходят до углов и
  ;; противоположные стороны принадлежат разным блокам.
  (setq axes (mark:fill-owned-axes hits)
        vs (car axes)
        hs (cadr axes)
        *mark:h-bins* (mark:fill-index-hs hs)
        cols (mark:fill-cluster-items vs)
        gap (+ *mark:fill-tol* 25.0)
        boxes nil
        n-own 0
        n-open 0
        iL 0
        nL (length cols))
  (while (< iL nL)
    (setq iR (1+ iL))
    (while (< iR nL)
      (setq Lcol (nth iL cols)
            Rcol (nth iR cols)
            oL (mark:fill-owners Lcol)
            oR (mark:fill-owners Rcol)
            xL (mark:fill-inner-edge Lcol 1.0)
            xR (mark:fill-inner-edge Rcol -1.0)
            w (if (and xL xR) (- xR xL) 0.0))
      (cond
        ((not (mark:fill-owners-differ oL oR))
         (setq n-own (1+ n-own)))
        ((or (<= w 50.0)
             (and (numberp *mark:fill-max-w*) (> w *mark:fill-max-w*)))
         (if (and (numberp *mark:fill-max-w*) (> w *mark:fill-max-w*))
           (setq iR nL))
         nil)
        (t
         (setq spanning (mark:fill-span-items
                          (mark:fill-hs-near *mark:h-bins* xL xR gap)
                          xL xR gap)
               rows (mark:fill-cluster-items spanning)
               iB 0
               nB (length rows))
         (while (< iB (1- nB))
           (setq bot (nth iB rows)
                 top (nth (1+ iB) rows)
                 yB (mark:fill-inner-edge bot 1.0)
                 yT (mark:fill-inner-edge top -1.0)
                 h (if (and yB yT) (- yT yB) 0.0))
           (cond
             ((not (mark:fill-owners-differ
                     (mark:fill-owners bot)
                     (mark:fill-owners top)))
              (setq n-own (1+ n-own)))
             ((or (<= h 50.0)
                  (and (numberp *mark:fill-max-h*) (> h *mark:fill-max-h*)))
              nil)
             ((not (and (mark:fill-covers (mark:fill-item-spans Lcol) yB yT gap)
                        (mark:fill-covers (mark:fill-item-spans Rcol) yB yT gap)))
              (setq n-open (1+ n-open)))
             ((mark:fill-v-split cols iL iR yB yT gap)
              (setq n-open (1+ n-open)))
             ((not (and (mark:fill-owners-differ
                          (mark:fill-overlap-owners Lcol yB yT gap)
                          (mark:fill-overlap-owners Rcol yB yT gap))
                        (mark:fill-owners-differ
                          (mark:fill-overlap-owners bot xL xR gap)
                          (mark:fill-overlap-owners top xL xR gap))))
              (setq n-own (1+ n-own)))
             (t
              (setq boxes (cons (list xL yB xR yT) boxes))))
           (setq iB (1+ iB)))))
      (setq iR (1+ iR)))
    (setq iL (1+ iL)))
  (mark:out
    (strcat "[INFO] Замкнутых ячеек: " (itoa (length boxes))
            ". Пропуск: один блок " (itoa n-own)
            ", неполный контур " (itoa n-open) "."))
  (if (null boxes)
    (mark:out "[INFO] Незамкнутый контур и камеры профиля не заполняю."))
  (reverse boxes))

(defun mark:fill-mode-grid-dyn (cells pts / hits boxes bb r)
  (mark:out "Сетка-динамика: выберите стойки и ригели. Только замкнутый контур.")
  (setq hits (mark:fill-pick-frame))
  (if (null hits)
    (progn
      (mark:out "[INFO] Выбор отменён.")
      (list cells pts))
    (progn
      (setq boxes (mark:fill-closed-cells hits))
      (foreach bb boxes
        (setq r (mark:fill-add cells pts bb)
              cells (car r)
              pts (cadr r)))
      (list cells pts))))

(defun mark:fill-mode-grid (cells pts / ss i e r segs total bb typ ed)
  (mark:out "Сетка-мультилинии: выберите мультилинии. Блоки стоек не нужны.")
  (setq ss (vl-catch-all-apply 'ssget (list (list (cons 0 "MLINE,LINE,ARC")))))
  (if (or (vl-catch-all-error-p ss) (null ss))
    (progn
      (mark:out "[INFO] Выбор отменён.")
      (list cells pts))
    (progn
      (setq i     (sslength ss)
            total i
            segs  nil)
      (mark:fill-type-stat ss)
      (repeat i
        (setq i   (1- i)
              e   (ssname ss i)
              ed  (entget e)
              typ (if ed (cdr (assoc 0 ed)) nil))
        ;; Игнорируем полилинии в режиме сетки
        (if (not (or (= typ "LWPOLYLINE") (= typ "POLYLINE")))
          (progn
            (setq r (vl-catch-all-apply 'mark:fill-extract-segs (list e)))
            (if (and (not (vl-catch-all-error-p r)) r)
              (setq segs (append segs r))))))
      (mark:out
        (strcat "[INFO] Извлечено отрезков: " (itoa (length segs))
                " из " (itoa total)))
      (if (null segs)
        (progn
          (mark:out "[WARN] Отрезки не извлечены — см. типы выше.")
          (list cells pts))
        (progn
          (setq r (mark:fill-segs->cells segs))
          (mark:out
            (strcat "[INFO] Сетка: осей X " (itoa (cadr r))
                    " Y " (itoa (caddr r))
                    "  ячеек " (itoa (length (car r)))))
          (if (null (car r))
            (progn
              (mark:out
                "[INFO] Мало осей — увеличьте *mark:fill-tol*.")
              (list cells pts))
            (progn
              (foreach bb (car r)
                (setq r    (mark:fill-add cells pts bb)
                      cells (car r)
                      pts   (cadr r)))
              (list cells pts))))))))

(defun mark:fill-apply (cells pts / doc space n ins x0 y0 x1 y1
                        w h obj lst cell t0 t1 t_ins)
  (if (null cells)
    (progn
      (mark:out "[INFO] Нет ячеек — вставок нет.")
      nil)
    (progn
      (setq doc   (mark:ax-get (vlax-get-acad-object) "ActiveDocument")
            space (if doc (mark:ax-get doc "ModelSpace") nil)
            n     0
            lst   nil)
      (if (null space)
        (progn
          (mark:out "[ERROR] ModelSpace недоступен.")
          (mark:note-error)
          nil)
        (progn
          (setq t0 (getvar "MILLISECS"))
          (setq *mark:fill-undo-off* nil
                *mark:fill-miss* nil)
          (if (and doc (not *mark:batch-undo*))
            (mark:ax-invoke-ok doc "StartUndoMark" nil))
          (foreach cell cells
            ;; Угол точный. Размер блока — кратно миллиметру.
            (setq x0 (float (nth 0 cell))
                  y0 (float (nth 1 cell))
                  x1 (float (nth 2 cell))
                  y1 (float (nth 3 cell))
                  w  (mark:round1 (- x1 x0))
                  h  (mark:round1 (- y1 y0)))
            (setq obj (mark:fill-insert space x0 y0))
            (if obj
              (progn
                (mark:fill-set-dims obj w h)
                (mark:fill-clear-attrs obj)
                (setq n   (1+ n)
                      ins (vl-catch-all-apply
                            'vlax-vla-object->ename (list obj)))
                (if (and (not (vl-catch-all-error-p ins)) ins)
                  (setq lst (cons ins lst)))
                (mark:out
                  (strcat "  вставка " (itoa n)
                          ": " (rtos x0 2 4) "," (rtos y0 2 4)
                          "  W=" (rtos w 2 0)
                          "  H=" (rtos h 2 0))))))
          (setq t1 (getvar "MILLISECS")
                t_ins (/ (- t1 t0) 1000.0))
          (if (and doc (not *mark:batch-undo*) (not *mark:fill-undo-off*))
            (mark:ax-invoke-ok doc "EndUndoMark" nil))
          (setq *mark:fill-undo-off* nil)
          (mark:out
            (strcat "[INFO] Вставлено заполнений: " (itoa n)
                    " (время вставки: " (rtos t_ins 2 2) " с, "
                    "всего: " (rtos (/ (- (getvar "MILLISECS") t0) 1000.0) 2 2) " с)"))
          (reverse lst))))))

(defun mark:fill-as-insert (e / ed)
  (setq ed (if e (entget e) nil))
  (if (and ed (= "INSERT" (cdr (assoc 0 ed))))
    e
    nil))

(defun mark:fill-pick-insert (/ sel e ed nest parents p typ)
  (mark:out "[INFO] Кликните линию каркаса блока, не пустую ячейку. Enter — пропуск.")
  (setq sel (vl-catch-all-apply 'entsel
              (list "\nЛиния блока <Enter — пропуск>: ")))
  (if (or (vl-catch-all-error-p sel) (null sel) (null (car sel)))
    nil
    (progn
      (setq e (mark:fill-as-insert (car sel)))
      (if (null e)
        (progn
          (setq nest (vl-catch-all-apply 'nentselp (list (cadr sel))))
          (if (and (not (vl-catch-all-error-p nest)) nest (listp nest) (cadddr nest))
            (progn
              (setq parents (cadddr nest)
                    p nil)
              (foreach x parents
                (setq p x))
              (setq e (mark:fill-as-insert p))))))
      (if e
        e
        (progn
          (setq ed (entget (car sel))
                typ (if ed (cdr (assoc 0 ed)) "?"))
          (mark:out (strcat "[WARN] Это не блок (" typ "). Нужна линия каркаса."))
          nil)))))

(defun mark:fill-exploded-segs (lst depth / segs ent en ed typ sub)
  (setq segs nil)
  (if (and lst (listp lst) (< depth 3))
    (foreach ent lst
      (if ent
        (progn
          (setq en (vl-catch-all-apply 'vlax-vla-object->ename (list ent))
                ed (if (and en (not (vl-catch-all-error-p en))) (entget en) nil)
                typ (if ed (cdr (assoc 0 ed)) nil))
          (cond
            ((member typ '("LINE" "ARC" "LWPOLYLINE" "POLYLINE" "MLINE"))
             (setq segs (append segs (mark:fill-local-segs en typ ed))))
            ((and (= typ "INSERT") (< depth 2))
             (setq sub (vl-catch-all-apply 'vlax-invoke (list ent "Explode")))
             (if (and (not (vl-catch-all-error-p sub)) (listp sub))
               (setq segs (append segs (mark:fill-exploded-segs sub (1+ depth)))))))
          (mark:fill-erase ent)))))
  segs)

(defun mark:fill-erase (obj / en)
  (if obj
    (progn
      (setq en (vl-catch-all-apply 'vlax-vla-object->ename (list obj)))
      (vl-catch-all-apply 'vla-Delete (list obj))
      (if (and en (not (vl-catch-all-error-p en)) (entget en))
        (vl-catch-all-apply 'entdel (list en)))
      (if (and en (not (vl-catch-all-error-p en)) (entget en))
        (setq *mark:copy-left*
          (1+ (if (numberp *mark:copy-left*) *mark:copy-left* 0)))))))

(defun mark:fill-explode-segs (e / doc obj copy lst segs)
  (setq doc (mark:ax-get (vlax-get-acad-object) "ActiveDocument")
        obj (mark:ax-catch-vla e)
        segs nil)
  (if (or (null doc) (null obj))
    nil
    (progn
      (if (and (null *mark:pt-undo*) (null *mark:no-expl-undo*))
        (mark:ax-invoke-ok doc "StartUndoMark" nil))
      (setq copy (vl-catch-all-apply 'vla-Copy (list obj)))
      (if (or (vl-catch-all-error-p copy) (null copy))
        (setq segs nil)
        (progn
          (setq lst (vl-catch-all-apply 'vlax-invoke (list copy "Explode")))
          (if (and (not (vl-catch-all-error-p lst)) (listp lst))
            (setq segs (mark:fill-exploded-segs lst 0))
            (setq segs nil))
          ;; Explode динблока часто не съедает копию. Иначе она остаётся под оригиналом.
          (mark:fill-erase copy)))
      (if (and (null *mark:pt-undo*) (null *mark:no-expl-undo*))
        (mark:ax-invoke-ok doc "EndUndoMark" nil))
      segs)))

(defun mark:fill-def-has-mline (bname depth / rec e ed typ nm hit)
  (if (or (not (mark:strp bname)) (= bname "") (>= depth 4))
    nil
    (progn
      (setq rec (tblsearch "BLOCK" bname)
            e   (if rec (cdr (assoc -2 rec)) nil)
            hit nil)
      (while (and e (null hit)
                  (setq ed (entget e))
                  (/= "ENDBLK" (cdr (assoc 0 ed))))
        (setq typ (cdr (assoc 0 ed)))
        (cond
          ((= typ "MLINE") (setq hit t))
          ((= typ "INSERT")
           (setq nm (cdr (assoc 2 ed))
                 hit (if nm (mark:fill-def-has-mline nm (1+ depth)) nil))))
        (setq e (entnext e)))
      hit)))

(defun mark:fill-lines-hw0 (segs / s out)
  (setq out nil)
  (foreach s segs
    (setq out
      (cons (if (and (> (length s) 4) (numberp (nth 4 s)))
              s
              (list (nth 0 s) (nth 1 s) (nth 2 s) (nth 3 s) 0.0))
            out)))
  (reverse out))

(defun mark:fill-segs-of-ins (e / pair ed nm mat segs)
  ;; Мультилиния — как раньше, из определения, с полушириной.
  ;; Линии динблока — только текущая видимость, без отступа 25 мм.
  (setq pair (assoc e *mark:seg-cache*))
  (if pair
    (cdr pair)
    (progn
      (setq ed  (entget e)
            nm  (if ed (cdr (assoc 2 ed)) nil)
            mat (if ed (mark:fill-mat-of ed) nil))
      (if (mark:fill-mline-p nm)
        (setq segs (if mat (mark:fill-def-segs nm mat 0) nil))
        (progn
          (if (and nm mat (= "*" (substr nm 1 1)))
            (setq segs (mark:fill-segs-xform (mark:fill-bname-local nm) mat)))
          (if (or (null segs) (< (length segs) 2))
            (progn
              (setq segs (mark:fill-lines-hw0 (mark:fill-explode-segs e))
                    *mark:expl-n* (1+ (if (numberp *mark:expl-n*) *mark:expl-n* 0)))
              (if (and (or (null segs) (< (length segs) 2)) nm mat)
                (setq segs (mark:fill-lines-hw0 (mark:fill-def-segs nm mat 0)))))
            (setq *mark:def-n* (1+ (if (numberp *mark:def-n*) *mark:def-n* 0))))
          (if (and (null *mark:dyn-quiet*) (or (null segs) (< (length segs) 2)))
            (mark:out "[WARN] Видимый каркас не прочитан — линии из определения, отступ 0."))))
      (if (null *mark:dyn-quiet*)
        (mark:out
          (strcat "[INFO] Блок «" (mark:fill-eff-name e)
                  "»: отрезков " (itoa (length segs))
                  (if (and nm (mark:fill-def-has-mline nm 0))
                    ""
                    ", видимые линии, отступ 0"))))
      (setq *mark:seg-cache* (cons (cons e segs) *mark:seg-cache*))
      segs)))

(defun mark:fill-try-rays (segs pt cells pts / bb)
  (setq bb (mark:fill-cell-by-rays segs pt))
  (cond
    ((and bb (mark:fill-box-closed segs bb))
     (mark:out "[INFO] Внутренний контур ячейки найден лучами.")
     (mark:fill-add cells pts bb))
    (bb
     (mark:out "[INFO] Лучи нашли незамкнутый контур — заполнение не ставлю.")
     nil)
    (t nil)))

(defun mark:fill-hit-cell (hits pt cells pts / boxes bb)
  ;; Та же ячейка, что в Сетка-динамика: стороны могут быть из нескольких линий.
  (setq boxes (mark:fill-closed-cells hits)
        bb (if boxes (mark:fill-smallest-cell boxes pt) nil))
  (cond
    (bb
     (mark:out "[INFO] Ячейка из замкнутого контура блоков.")
     (mark:fill-add cells pts bb))
    (boxes
     (mark:out "[INFO] Замкнутые ячейки есть, но точка вне их.")
     nil)
    (t nil)))

(defun mark:fill-dyn-from (hits pt cells pts / r segs)
  (setq r nil)
  (if hits
    (setq r (mark:fill-hit-cell hits pt cells pts)))
  (if (null r)
    (progn
      (setq segs (mark:fill-hits-segs hits))
      (mark:out
        (strcat "[INFO] Отрезков каркаса: " (itoa (length segs))))
      (if (>= (length segs) 4)
        (setq r (mark:fill-try-rays segs pt cells pts)))))
  r)

(defun mark:fill-mline-p (bname / pair hit)
  (if (or (null bname) (= bname ""))
    nil
    (progn
      (setq pair (assoc bname *mark:mline-cache*))
      (if pair
        (eq (cdr pair) 'yes)
        (progn
          (setq hit (mark:fill-def-has-mline bname 0))
          (setq *mark:mline-cache*
            (cons (cons bname (if hit 'yes 'no)) *mark:mline-cache*))
          hit)))))

(defun mark:fill-bname-local (bname / pair segs)
  (setq pair (assoc bname *mark:udef-cache*))
  (if pair
    (cdr pair)
    (progn
      (setq segs (mark:fill-lines-hw0
                   (mark:fill-def-segs bname (list 1.0 0.0 0.0 0.0 1.0 0.0) 0)))
      (setq *mark:udef-cache* (cons (cons bname segs) *mark:udef-cache*))
      segs)))

(defun mark:fill-bin (x)
  (fix (/ x 2000.0)))

(defun mark:fill-index-hs (hs / acc it k0 k1 k bins item pk p)
  (setq acc nil)
  (foreach it hs
    (setq k0 (mark:fill-bin (nth 1 it))
          k1 (mark:fill-bin (nth 2 it))
          k k0)
    (if (> k k1)
      (setq pk k k k1 k1 pk))
    (while (<= k k1)
      (setq acc (cons (cons k it) acc)
            k (1+ k))))
  (if (null acc)
    nil
    (progn
      (setq acc (vl-sort acc '(lambda (a b) (< (car a) (car b))))
            bins nil
            item nil
            k nil)
      (foreach p acc
        (if (and k (= (car p) k))
          (setq item (cons (cdr p) item))
          (progn
            (if k (setq bins (cons (cons k item) bins)))
            (setq k (car p)
                  item (list (cdr p))))))
      (if k (setq bins (cons (cons k item) bins)))
      bins)))

(defun mark:fill-hs-near (bins xL xR gap / k0 k1 k b out)
  (if (null bins)
    nil
    (progn
      (setq k0 (mark:fill-bin (- xL gap))
            k1 (mark:fill-bin (+ xR gap))
            k k0
            out nil)
      (while (<= k k1)
        (setq b (assoc k bins))
        (if b (setq out (append (cdr b) out)))
        (setq k (1+ k)))
      out)))

(defun mark:fill-hits-merge (a b / out e seen)
  (setq out nil
        seen nil)
  (foreach h (append a b)
    (setq e (cadr h))
    (if (not (member e seen))
      (setq seen (cons e seen)
            out (cons h out))))
  (reverse out))

(defun mark:fill-hit-rank (pt e / bb)
  ;; Огромный габарит динблока не должен вытеснять соседние стойки.
  (setq bb (mark:cell-bb e))
  (if (and bb
           (or (> (abs (- (nth 2 bb) (nth 0 bb))) 8000.0)
               (> (abs (- (nth 3 bb) (nth 1 bb))) 8000.0)))
    (mark:fill-ins-dist pt e)
    (mark:fill-bb-dist pt e)))

(defun mark:fill-dyn-candidates (pt win / ss i e nm d hits)
  (mark:out
    (strcat "[INFO] Ищу вставки в пределах " (rtos win 2 0) " мм…"))
  (setq ss (vl-catch-all-apply 'ssget
             (list "X" (list (cons 0 "INSERT"))))
        hits nil)
  (if (and ss (not (vl-catch-all-error-p ss)))
    (progn
      (setq i (sslength ss))
      (repeat i
        (setq i (1- i)
              e (ssname ss i)
              nm (mark:fill-eff-name e))
        (if (not (mark:fill-skip-block? nm))
          (progn
            (setq d (mark:fill-hit-rank pt e))
            (if (<= d win)
              (setq hits (cons (list d e) hits))))))))
  (vl-sort hits '(lambda (a b) (< (car a) (car b)))))

(defun mark:fill-bb-area (e / bb)
  (setq bb (mark:cell-bb e))
  (if bb
    (* (abs (- (nth 2 bb) (nth 0 bb)))
       (abs (- (nth 3 bb) (nth 1 bb))))
    1.0e12))

(defun mark:fill-dyn-pick (hits)
  ;; Габарит динблока больше ячейки. По площади не отсекать — иначе беру 0.
  (mark:fill-hits-take hits 80))

(defun mark:fill-dyn-cell (ptu ptw cells pts / all hits r)
  ;; Ячейка — пустота между вставками, не одна стойка.
  (setq r nil)
  (if *mark:dyn-set*
    (progn
      (mark:out "[INFO] Пробую прежний каркас.")
      (setq r (mark:fill-dyn-from *mark:dyn-set* ptw cells pts)
            hits *mark:dyn-set*)))
  (if (null r)
    (progn
      (setq all (mark:fill-dyn-candidates ptw 6000.0)
            hits (mark:fill-dyn-pick all))
      (mark:out
        (strcat "[INFO] В пределах 6000 мм: " (itoa (length all))
                ", беру " (itoa (length hits))))
      (mark:fill-hit-report hits)
      (setq r (mark:fill-dyn-from hits ptw cells pts))))
  (if r
    (progn
      (setq *mark:dyn-set* hits)
      r)
    (progn
      (mark:out "[INFO] Ячейка между блоками каркаса не собрана.")
      (list cells pts))))

(defun mark:fill-mode-dyn (cells pts / pt)
  (setq pt (getpoint "\nТочка-динамика внутри ячейки <Enter — конец>: "))
  (if (null pt)
    (progn
      (mark:out "[INFO] Конец режима «Точка-динамика».")
      (list nil nil 'cancel))
    (mark:fill-dyn-cell pt (mark:fill-pt-wcs pt) cells pts)))

(defun mark:fill-pt-undo-end (/ doc)
  (if *mark:pt-undo*
    (progn
      (setq doc (mark:ax-get (vlax-get-acad-object) "ActiveDocument"))
      (if doc (mark:ax-invoke-ok doc "EndUndoMark" nil))))
  (setq *mark:pt-undo* nil
        *mark:batch-undo* nil))

(defun mark:fill-pt-err (msg)
  (mark:fill-pt-undo-end)
  (setq *error* *mark:pt-olderr*)
  (if (and msg
           (/= msg "Function cancelled")
           (/= msg "quit / exit abort"))
    (princ (strcat "\n" msg)))
  (princ))

(defun mark:fill-points-loop (modefn start done / going r n ins one acc-cells acc-pts doc)
  ;; Один UNDO на весь заход: все вставки режима «точка».
  (setq *mark:pt-olderr* *error*
        *error* mark:fill-pt-err
        doc (mark:ax-get (vlax-get-acad-object) "ActiveDocument")
        *mark:batch-undo* t
        *mark:pt-undo* nil)
  (if doc
    (progn
      (mark:ax-invoke-ok doc "StartUndoMark" nil)
      (setq *mark:pt-undo* t)))
  (setq going     t
        n         0
        ins       nil
        acc-cells nil
        acc-pts   nil)
  (mark:out start)
  (mark:out "[INFO] UNDO снимет все вставки этого захода разом.")
  (while going
    (setq r (apply modefn (list acc-cells acc-pts)))
    (cond
      ((or (null r) (eq (caddr r) 'cancel))
       (setq going nil))
      ((> (length (car r)) (length acc-cells))
       (setq one (mark:fill-apply (list (car (car r))) (cadr r))
             acc-cells (car r)
             acc-pts   (cadr r))
       (if one
         (setq ins (append ins one)
               n   (length ins))))
      (t nil)))
  (mark:fill-pt-undo-end)
  (setq *error* *mark:pt-olderr*)
  (mark:out (strcat done (itoa n)))
  ins)

(defun mark:fill-ask-mode (/ kw mode)
  ;; Два ключа на «Точка» или «Сетка» нельзя: AutoCAD берёт первый.
  ;; Поэтому динамика в списке с цифрой. Команда Точка-динамика — отдельно.
  (mark:out "Точка-динамика: команда, либо 4, либо Д. Буква Т — Точка-мультилинии.")
  (initget (strcat "Сетка-мультилинии Точка-мультилинии "
                   "3-Сетка-динамика 4-Точка-динамика Полилинии "
                   "Динамика-точка Мультилинии-точка 1 2 5"))
  (setq kw (getkword
    "\nРежим [Сетка-мультилинии/Точка-мультилинии/3-Сетка-динамика/4-Точка-динамика/Полилинии] <Сетка-мультилинии>: "))
  (cond
    ((or (null kw) (= kw "Сетка-мультилинии") (= kw "1"))
     (setq mode "3"))
    ((or (= kw "Точка-мультилинии") (= kw "Мультилинии-точка") (= kw "2"))
     (setq mode "2"))
    ((or (= kw "3-Сетка-динамика") (= kw "3"))
     (setq mode "5"))
    ((or (= kw "4-Точка-динамика") (= kw "4")
         (= kw "Динамика-точка") (= kw "Д"))
     (setq mode "4"))
    ((or (= kw "Полилинии") (= kw "5"))
     (setq mode "1"))
    (t (setq mode "3")))
  (mark:out
    (strcat "[INFO] Ключ: " (if kw kw "<Enter>")
            " = "
            (cond ((= mode "2") "Точка-мультилинии")
                  ((= mode "5") "Сетка-динамика")
                  ((= mode "4") "Точка-динамика")
                  ((= mode "1") "Полилинии")
                  (t "Сетка-мультилинии"))))
  mode)

(defun mark:fill-main (/ kw mode cells pts r t_geom_start t_geom ins-list forced)
  (setq forced *mark:fill-force*
        *mark:fill-force* nil)
  (mark:cmd-line
    "МАРКАБЛОК — вставка «Заполнение в витраж». Список: Сетка-мультилинии, Точка-мультилинии, Сетка-динамика, Точка-динамика, Полилинии. Марки не пишет.")
  (mark:reset-state)
  (mark:banner)
  (mark:out "МАРКАБЛОК — вставка «Заполнение в витраж» по ячейкам")
  (setq mode (if forced forced (mark:fill-ask-mode)))
  (if forced
    (mark:out
      (strcat "[INFO] Команда = "
              (cond ((= mode "2") "Точка-мультилинии")
                    ((= mode "5") "Сетка-динамика")
                    ((= mode "4") "Точка-динамика")
                    ((= mode "1") "Полилинии")
                    (t "Сетка-мультилинии")))))
  (setq cells nil
        pts   nil
        ins-list nil
        t_geom_start (getvar "MILLISECS"))
  (cond
    ((= mode "2")
     (setq ins-list
       (mark:fill-points-loop
         'mark:fill-mode-point
         "[INFO] Точка-мультилинии: ячейка за ячейкой. Enter — конец."
         "[INFO] Режим «Точка-мультилинии» завершён. Вставлено: ")))
    ((= mode "4")
     (setq *mark:dyn-ins* nil
           *mark:dyn-set* nil
           *mark:dyn-bb*  nil
           ins-list
       (mark:fill-points-loop
         'mark:fill-mode-dyn
         "[INFO] Точка-динамика: ячейка из блоков каркаса. Enter — конец."
         "[INFO] Режим «Точка-динамика» завершён. Вставлено: "))
     (setq *mark:dyn-ins* nil
           *mark:dyn-set* nil
           *mark:dyn-bb*  nil))
    ((= mode "5")
     (setq r        (mark:fill-mode-grid-dyn cells pts)
           cells    (car r)
           pts      (cadr r)
           t_geom   (/ (- (getvar "MILLISECS") t_geom_start) 1000.0)
           ins-list (mark:fill-apply cells pts))
     (if cells
       (mark:out (strcat "[ТАЙМИНГ] Расчет геометрии сетки: " (rtos t_geom 2 2) " с"))))
    ((= mode "3")
     (setq r        (mark:fill-mode-grid cells pts)
           cells    (car r)
           pts      (cadr r)
           t_geom   (/ (- (getvar "MILLISECS") t_geom_start) 1000.0)
           ins-list (mark:fill-apply cells pts))
     (if cells
       (mark:out (strcat "[ТАЙМИНГ] Расчет геометрии сетки: " (rtos t_geom 2 2) " с"))))
    (t
     (setq r        (mark:fill-mode-poly cells pts)
           cells    (car r)
           pts      (cadr r)
           ins-list (mark:fill-apply cells pts))))
  (mark:out (strcat "Ошибок: " (itoa *mark:errors*)))
  (mark:out (strcat "Предупреждений: " (itoa *mark:warnings*)))
  (setq *mark:fills* ins-list)
  (princ))
(defun mark:a-all (/ kw mode doc r cells pts new-fills)
  (mark:cmd-line
    "МАРКА — полный цикл. Сетка: вставка, марки, рядовка, ведомость. Заполнения: без вставки.")
  (mark:reset-state)
  (mark:out "========================================")
  (mark:out (strcat " МАРКА — универсальный пакет  " *mark:rev*))
  (mark:out "========================================")
  (initget "Сетка Заполнения С З S Z 1 2")
  (setq kw (getkword "\nИсточник [Сетка/Заполнения] <Сетка>: "))
  (cond
    ((or (null kw) (= kw "Сетка") (= kw "С") (= kw "S") (= kw "1"))
     (setq mode "grid"))
    (t
     (setq mode "blocks")))
  (setq doc (mark:ax-get (vlax-get-acad-object) "ActiveDocument"))
  (if doc
    (mark:ax-invoke-ok doc "StartUndoMark" nil))
  (setq *mark:batch-undo* t)
  (if (eq mode "grid")
    (progn
      ;; ВАРИАНТ 1: ПО СЕТКЕ (полный цикл: МАРКАБЛОК -> МАРКАРОВКА -> МАРКАРЯД -> МАРКАТАБЛ)
      (mark:out "[ЭТАП 1/4] МАРКАБЛОК — раскладка заполнений по сетке витража...")
      (setq r (mark:fill-mode-grid nil nil)
            cells (car r)
            pts   (cadr r))
      (if (null cells)
        (mark:out "[INFO] Сетка не выбрана или ячеек нет — пакет прерван.")
        (progn
          (setq new-fills (mark:fill-apply cells pts))
          (if new-fills
            (progn
              (setq *mark:fills* new-fills
                    *mark:reuse-sel* t
                    *mark:sel-total* (length new-fills))
              ;; МАРКАРОВКА
              (mark:out "[ЭТАП 2/4] МАРКАРОВКА — маркировка заполнений...")
              (mark:main)
              ;; МАРКАРЯД
              (mark:out "[ЭТАП 3/4] МАРКАРЯД — расстановка рядовки...")
              (mark:ar-main)
              ;; МАРКАТАБЛ
              (mark:out "[ЭТАП 4/4] МАРКАТАБЛ — ведомость заполнения...")
              (mtab:main))))))
    (progn
      ;; ВАРИАНТ 2: ИЗ ГОТОВЫХ БЛОКОВ (МАРКАРОВКА -> МАРКАРЯД -> МАРКАТАБЛ)
      (mark:out "[ЭТАП 1/3] МАРКАРОВКА — маркировка готовых блоков...")
      (setq *mark:reuse-sel* nil)
      (mark:main)
      (if *mark:fills*
        (progn
          (setq *mark:reuse-sel* t)
          (mark:out "[ЭТАП 2/3] МАРКАРЯД — расстановка рядовки...")
          (mark:ar-main)
          (mark:out "[ЭТАП 3/3] МАРКАТАБЛ — ведомость заполнения...")
          (mtab:main))
        (mark:out "[INFO] Нет заполнений для обработки — пакет прерван."))))
  (if doc
    (mark:ax-invoke-ok doc "EndUndoMark" nil))
  (setq *mark:reuse-sel*  nil
        *mark:batch-undo* nil)
  (princ))
(defun c:МАРКАРОВКА () (mark:main))
(defun c:MARKZ () (mark:main))
(defun c:МАРКАРЯД () (mark:ar-main))
(defun c:MARKAR () (mark:ar-main))
(defun c:MARKA () (mark:a-all))
(defun c:МАРКА () (mark:a-all))

(defun mark:test-line (x0 y0 x1 y1)
  (entmake (list '(0 . "LINE")
                 (list 10 x0 y0 0.0)
                 (list 11 x1 y1 0.0))))

(defun mark:test-purge (nm / ss i e doc blocks blk)
  (setq ss (vl-catch-all-apply 'ssget
             (list "X" (list '(0 . "INSERT") (cons 2 nm)))))
  (if (and ss (not (vl-catch-all-error-p ss)))
    (progn
      (setq i (sslength ss))
      (repeat i
        (setq i (1- i))
        (entdel (ssname ss i)))))
  (setq doc (mark:ax-get (vlax-get-acad-object) "ActiveDocument"))
  (if doc
    (progn
      (setq blocks (mark:ax-get doc "Blocks")
            blk (if blocks
                  (vl-catch-all-apply 'vla-item (list blocks nm))
                  nil))
      (if (and blk (not (vl-catch-all-error-p blk)))
        (vl-catch-all-apply 'vla-delete (list blk)))))
  t)

(defun mark:test-make-block (/ r)
  (setq r (entmake (list '(0 . "BLOCK")
                         '(2 . "MARKZ_TEST_CELL")
                         '(70 . 0)
                         (list 10 0.0 0.0 0.0))))
  (if (null r)
    nil
    (progn
      (mark:test-line 0.0 0.0 2000.0 0.0)
      (mark:test-line 0.0 1000.0 2000.0 1000.0)
      (mark:test-line 0.0 0.0 0.0 1000.0)
      (mark:test-line 2000.0 0.0 2000.0 1000.0)
      (mark:test-line 800.0 0.0 800.0 1000.0)
      (entmake '((0 . "ENDBLK")))
      t)))

(defun mark:test-insert (nm x y / r)
  (setq r (entmake (list '(0 . "INSERT")
                         (cons 2 nm)
                         (list 10 x y 0.0)
                         '(41 . 1.0)
                         '(42 . 1.0)
                         '(50 . 0.0))))
  (if (and r (listp r) (assoc -1 r))
    (cdr (assoc -1 r))
    (if r (entlast) nil)))

(defun c:МАРКАТЕСТ (/ doc e segs bb w h ok x y hits found)
  (mark:out "========================================")
  (mark:out (strcat " ТЕСТ ЯЧЕЙКИ БЛОКА  " *mark:rev*))
  (mark:out "Прямоугольник 2000x1000, стойка на 800. Ожидание: W=800 H=1000")
  (mark:out "========================================")
  (setq doc (mark:ax-get (vlax-get-acad-object) "ActiveDocument")
        ok nil)
  (if doc (mark:ax-invoke-ok doc "StartUndoMark" nil))
  (mark:test-purge "MARKZ_TEST_CELL")
  (if (not (mark:test-make-block))
    (mark:out "[TEST] Блок не создан — FAIL")
    (progn
      (setq e (mark:test-insert "MARKZ_TEST_CELL" 100000.0 100000.0))
      (if (null e)
        (mark:out "[TEST] Вставка не создана — FAIL")
        (progn
          (setq x 100400.0
                y 100500.0
                segs (mark:fill-segs-of-ins e)
                bb (if segs (mark:fill-cell-by-rays segs (list x y)) nil))
          (mark:out (strcat "[TEST] Отрезков из блока: " (itoa (length segs))))
          (if (null bb)
            (mark:out "[TEST] Ячейка по отрезкам блока — FAIL")
            (progn
              (setq w (- (nth 2 bb) (nth 0 bb))
                    h (- (nth 3 bb) (nth 1 bb))
                    ok (and (> w 700.0) (< w 900.0)
                            (> h 900.0) (< h 1100.0)))
              (mark:out
                (strcat "[TEST] Ячейка W=" (rtos w 2 1)
                        " H=" (rtos h 2 1)
                        (if ok " — OK" " — FAIL")))))
          (setq hits (mark:fill-insert-hits (list x y) (list x y))
                found nil)
          (foreach hit hits
            (if (eq (cadr hit) e) (setq found t)))
          (mark:out
            (strcat "[TEST] Окно нашло тестовый блок: "
                    (if found "OK" "FAIL")))
          (if (null found) (setq ok nil))
          (entdel e)))))
  (mark:test-purge "MARKZ_TEST_CELL")
  (if doc (mark:ax-invoke-ok doc "EndUndoMark" nil))
  (mark:out (if ok "[TEST] Итог — OK" "[TEST] Итог — FAIL"))
  (princ))


(defun mark:hex-val (s / i n c)
  (setq i 1
        n 0)
  (while (and s (<= i (strlen s)))
    (setq c (ascii (substr s i 1))
          n (+ (* n 16)
               (cond
                 ((and (>= c 48) (<= c 57)) (- c 48))
                 ((and (>= c 65) (<= c 70)) (- c 55))
                 ((and (>= c 97) (<= c 102)) (- c 87))
                 (t 0)))
          i (1+ i)))
  n)

(defun mark:copy-key (e / ed p nm rot sx sy)
  (setq ed (entget e)
        p (if ed (cdr (assoc 10 ed)) nil)
        nm (mark:fill-eff-name e))
  (if (or (null p) (null (car p)) (mark:fill-skip-block? nm))
    nil
    (progn
      (setq rot (cdr (assoc 50 ed))
            sx (cdr (assoc 41 ed))
            sy (cdr (assoc 42 ed)))
      (strcat (if nm nm "?")
              "|" (rtos (float (car p)) 2 2)
              "|" (rtos (float (cadr p)) 2 2)
              "|" (rtos (if (numberp rot) rot 0.0) 2 4)
              "|" (rtos (if (numberp sx) sx 1.0) 2 4)
              "|" (rtos (if (numberp sy) sy 1.0) 2 4)))))

(defun c:МАРКАКОПИИ (/ ss i e key acc groups g keep n doc gone)
  ;; Копии чтения каркаса лежат на оригинале. Оставляет одну вставку на точку.
  (mark:out "МАРКАКОПИИ — убрать копии блоков с той же точкой вставки.")
  (setq ss (vl-catch-all-apply 'ssget (list "X" (list (cons 0 "INSERT"))))
        acc nil
        n 0)
  (if (or (vl-catch-all-error-p ss) (null ss))
    (mark:out "[INFO] Вставок нет.")
    (progn
      (setq i (sslength ss))
      (mark:out (strcat "[INFO] Вставок в чертеже: " (itoa i)))
      (repeat i
        (setq i (1- i)
              e (ssname ss i)
              key (mark:copy-key e))
        (if key
          (setq acc (cons (cons key e) acc))))
      (setq acc (vl-sort acc '(lambda (a b) (< (car a) (car b))))
            groups nil
            g nil
            key nil)
      (foreach pair acc
        (if (and key (= (car pair) key))
          (setq g (cons (cdr pair) g))
          (progn
            (if (and key (> (length g) 1))
              (setq groups (cons g groups)))
            (setq key (car pair)
                  g (list (cdr pair))))))
      (if (and key (> (length g) 1))
        (setq groups (cons g groups)))
      (setq doc (mark:ax-get (vlax-get-acad-object) "ActiveDocument"))
      (if doc (mark:ax-invoke-ok doc "StartUndoMark" nil))
      (foreach g groups
        (setq keep nil)
        (foreach e g
          (if (or (null keep)
                  (< (mark:hex-val (cdr (assoc 5 (entget e))))
                     (mark:hex-val (cdr (assoc 5 (entget keep))))))
            (setq keep e)))
        (foreach e g
          (if (not (eq e keep))
            (progn
              (setq gone (vl-catch-all-apply 'entdel (list e)))
              (if (not (vl-catch-all-error-p gone))
                (setq n (1+ n)))))))
      (if doc (mark:ax-invoke-ok doc "EndUndoMark" nil))
      (mark:out
        (strcat "[INFO] Удалено лишних копий: " (itoa n)
                ". На каждой точке оставлена одна вставка."))))
  (princ))

(defun c:МАРКАБЛОК () (mark:fill-main))
(defun c:MARKFILL () (mark:fill-main))
(defun c:Сетка-мультилинии () (setq *mark:fill-force* "3") (c:МАРКАБЛОК))
(defun c:Точка-мультилинии () (setq *mark:fill-force* "2") (c:МАРКАБЛОК))
(defun c:Сетка-динамика () (setq *mark:fill-force* "5") (c:МАРКАБЛОК))
(defun c:Точка-динамика () (setq *mark:fill-force* "4") (c:МАРКАБЛОК))
(defun c:Полилинии () (setq *mark:fill-force* "1") (c:МАРКАБЛОК))

;; Снять старые имена из памяти AutoCAD при повторной загрузке
(setq c:МАРКИРОВКА nil)
(setq c:МАРКАЗ nil)
(setq c:МАРКАР nil)
(setq c:МАРКАЗАЛ nil)
(setq c:МАРКАЗАЛА nil)

(defun mark:snapshot-path (/ src dir ch)
  (setq src (mark:find-source)
        dir (if src (vl-filename-directory src) nil))
  (if (or (null dir) (= dir ""))
    (setq dir (getvar "DWGPREFIX")))
  (if (and (mark:strp dir) (/= dir ""))
    (progn
      (setq ch (substr dir (strlen dir) 1))
      (if (and (/= ch "/") (/= ch "\\"))
        (setq dir (strcat dir "/"))))
    (setq dir ""))
  (strcat dir "MARKZ_SNAPSHOT.txt"))

(defun c:SNAPSHOT (/ ss i e bb lst fn f ed typ p0 p1 rad w h)
  (prompt "\nВыберите элементы для снятия геометрии (SNAPSHOT): ")
  (setq ss (ssget))
  (if (null ss)
    (prompt "\n[INFO] Ничего не выбрано.")
    (progn
      (setq i (sslength ss)
            lst nil)
      (repeat i
        (setq i (1- i)
              e (ssname ss i)
              ed (entget e)
              typ (cdr (assoc 0 ed))
              bb (mark:fill-e-bb e))
        (setq lst (cons (list typ
                              (if bb (mapcar 'mark:round1 bb) nil))
                        lst)))
      (setq fn (mark:snapshot-path)
            f  (open fn "w"))
      (if f
        (progn
          (write-line (strcat "; SNAPSHOT: " (itoa (length lst)) " элементов") f)
          (foreach item lst
            (write-line (vl-prin1-to-string item) f))
          (close f)
          (prompt (strcat "\n[OK] Снапшот геометрии сохранен в " fn "\n")))
        (prompt (strcat "\n[ERROR] Не удалось открыть файл " fn "\n")))))
  (princ))

;;; ---- TEST 00 + сообщение загрузки ------------------------------------

(mark:test-parens)
(princ (strcat
  "\n[MARKZ] Загружен " *mark:rev*
  "\nМАРКА       — полный цикл: сетка или готовые заполнения"
  "\nМАРКАРОВКА  — марки блоков в «Заполнение в витраж»"
  "\nМАРКАРЯД    — рядовка из блоков «Ряд заполнений»: номера снизу, буквы справа"
  "\nМАРКАТАБЛ   — ведомость"
  "\nМАРКАБЛОК   — Сетка-мультилинии, Точка-мультилинии, Сетка-динамика, Точка-динамика, Полилинии"
  "\nТочка-динамика — отдельная команда; в списке: 4 или Д"
  "\nМАРКАТЕСТ   — проверка ячейки на тестовом блоке из линий"
  "\nМАРКАКОПИИ — убрать копии динблоков, оставшиеся на оригиналах\n"))
(princ)
