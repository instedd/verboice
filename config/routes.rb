Verboice::Application.routes.draw do

  resources :contacts do
    resources :persisted_variables
  end

  resources :channels do
    resources :queued_calls
    member do
      get :call
    end
  end

  match '/' => 'home#index',  :as => 'home'

  devise_for :accounts

  resources :applications do
    member do
      get :edit_workflow
      put :update_workflow
      get :play_recording
      post :save_recording
    end
  end

  resources :call_logs, path: :calls do
    member do
      get :progress
    end
    collection do
      get :queued
      post :enqueue
    end
  end

  resources :call_queues, path: :queues

  match "api/call" => "api#call", :as => :api_call
  match "api/call/:id/state" => "api#call_state"
  match 'api/call/:id/redirect' => 'api#call_redirect'

  get "api/channels" => "api_channels#list"
  post  "api/channels" => "api_channels#create"
  delete "api/channels/:name" => "api_channels#destroy"

  get "api/call_queues" => "api_call_queues#index"
  get "api/call_queues/:name" => "api_call_queues#show"
  post "api/call_queues" => "api_call_queues#create"
  put "api/call_queues/:name" => "api_call_queues#update"
  delete "api/call_queues/:name" => "api_call_queues#destroy"

  root :to => 'home#index'

  get 'terms_and_conditions', :to => redirect('/')
end
