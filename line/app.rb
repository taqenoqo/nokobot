require 'sinatra'
require 'line/bot'
require 'rest-client'

configure do
  set :port, ENV["PORT"]
  set :bot_url, "http://#{ENV["BOT_HOST_NAME"]}:#{ENV["BOT_PORT"]}/bot"
end

def client
  @client ||= Line::Bot::Client.new do |config|
    config.channel_id = ENV["CHANNEL_ID"]
    config.channel_secret = ENV["CHANNEL_SECRET"]
    config.channel_token = ENV["CHANNEL_TOKEN"]
  end
end

post '/callback' do
  body = request.body.read

  signature = request.env['HTTP_X_LINE_SIGNATURE']
  unless client.validate_signature(body, signature)
    error 400 do 'Bad Request' end
  end

  events = client.parse_events_from(body)
  events.each do |event|
    case event
    when Line::Bot::Event::Message
      case event.type
      when Line::Bot::Event::MessageType::Text
        message = {
          type: 'text',
          text: event.message['text']
        }
        bot_input = event.message['text'].to_json
        response = RestClient.post(settings.bot_url, bot_input, content_type: :json, accept: :json)
        p response
        client.reply_message(event['replyToken'], message)
      end
    end
  end
  "OK"
end
