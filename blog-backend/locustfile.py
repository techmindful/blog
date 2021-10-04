import time
from locust import HttpUser, task, between

class User(HttpUser):

    wait_time = between(1, 5)

    @task
    def unicode_to_path(self):
        request = self.client.put(
            "/blog-apis/emojis-in-elm/unicode-to-path",
            data = b"123",
            headers = { "content-type": "text/plain;charset=utf-8" },
            catch_response = True,
        )

        with request as response:
            if response.status_code != 200:
                response.failure( "Non 200 status code." )

            # TODO: Handle JSON error.
            compiler_msg = response.json()["compilerError"]
            if "TYPE MISMATCH" not in compiler_msg:  
                response.failure( "Wrong compiler error message: " + compiler_msg )

