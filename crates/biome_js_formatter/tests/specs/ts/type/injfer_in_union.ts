type Type<T> = [T] extends [(infer S extends string) | undefined] ? S : T;