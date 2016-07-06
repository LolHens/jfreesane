package au.com.southsky.jfreesane;

import com.google.common.truth.DefaultSubject;
import com.google.common.truth.Subject;
import com.google.common.truth.Truth;

/**
 * Created by pierr on 06.07.2016.
 */
public class Truth2 {
    public static Subject<DefaultSubject, Object> assertThat(Object object) {
        return Truth.assertThat(object);
    }
}
