FROM ruby
ENV APP_DIR /opt/nokobot
WORKDIR $APP_DIR
COPY Gemfile Gemfile.lock ./
RUN bundle install
COPY src ./src
COPY bin ./bin
ENV PATH $APP_DIR/bin:$PATH
ENTRYPOINT ["chat"]

